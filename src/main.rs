use std::{
    fs::File,
    io::BufReader,
    num::{NonZero, NonZeroUsize},
    path::PathBuf,
    sync::Arc,
    thread,
};

use clap::Parser;
use crossbeam::channel::bounded;

use lexer_search_lib::{
    engine::{
        matcher::{FullMatch, Matcher},
        matchers::{Tries, make_c_like_lexer, make_python_like_lexer, make_rust_like_lexer},
    },
    io::{FullMatchOut, Language, final_postprocess},
    lexer::DEFAULT_MAX_TOKEN_LENGTH,
};

#[derive(Parser, Debug)]
#[command(name = "lexer-search")]
#[command(about = "Source code scanner")]
#[command(version = env!("BUILD_VERSION"))]
pub struct Args {
    #[cfg(not(feature = "embed-patterns"))]
    pub patterns_path: PathBuf,
    pub scan_path: PathBuf,
    /// max simultaneous independent overlapping partial matches
    #[arg(default_value_t = 5000)]
    pub max_concurrent_matches: usize,
    /// the maximum length of a token. if a token is larger than this specified
    /// value this will causes non ... sections of a pattern to not match
    #[arg(default_value_t = DEFAULT_MAX_TOKEN_LENGTH)]
    pub max_token_length: NonZeroUsize,
    /// should be some reasonably small number. specifies the max number of
    /// simultaneous groups
    #[arg(default_value_t = 10.try_into().unwrap())]
    pub max_concurrent_groups: NonZero<usize>,
    /// should be some reasonably small number. specifies the max number of
    /// matches per group
    #[arg(default_value_t = 10.try_into().unwrap())]
    pub max_group_size: NonZero<usize>,
}

#[cfg(feature = "embed-patterns")]
// '/' ok even on windows
const EMBEDDED_PATTERNS: &'static [u8] = include_bytes!("../target/lexer-search-embedded-patterns");

fn main() -> Result<(), String> {
    let args = Args::parse();

    // Load rules
    let tries = {
        #[cfg(not(feature = "embed-patterns"))]
        {
            Tries::construct_tries(&args.patterns_path, args.max_token_length)?
        }

        #[cfg(feature = "embed-patterns")]
        {
            let res =
                bincode::serde::decode_from_slice(EMBEDDED_PATTERNS, bincode::config::standard())
                    .expect("failed to load embedded patterns");
            res.0
        }
    };
    let tries = Arc::new(tries);

    let (tx, rx) = bounded::<PathBuf>(512);
    let scan_path = args.scan_path.clone();
    let producer = thread::spawn(move || {
        for entry in walkdir::WalkDir::new(scan_path)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
        {
            if tx.send(entry.path().to_path_buf()).is_err() {
                break;
            }
        }
    });

    let mut handles = Vec::new();
    for _ in 0..num_cpus::get() {
        let rx = rx.clone();
        let tries = Arc::clone(&tries);
        let scan_path = args.scan_path.clone();
        let handle = thread::spawn(move || {
            while let Ok(path) = rx.recv() {
                if let Some(ext) = path.extension() {
                    if let Some(lang) = Language::from_file_extension(ext) {
                        let file = match File::open(&path) {
                            Ok(f) => f,
                            Err(_) => continue,
                        };
                        let mut r = BufReader::new(file);
                        let trie = tries.trie_for_lang(lang);
                        let mut matcher = Matcher::new(
                            trie,
                            args.max_concurrent_matches,
                            args.max_token_length,
                            args.max_concurrent_groups,
                            args.max_group_size,
                        );

                        let write_out_finding = |m: FullMatch| {
                            if let Some(m) = final_postprocess(m) {
                                let out = FullMatchOut {
                                    file: path.strip_prefix(&scan_path).unwrap(),
                                    start: m.start,
                                    end: m.end,
                                    name: m.name,
                                    group: m.group,
                                    captures: &m.captures,
                                };
                                println!("{}", serde_json::to_string(&out).unwrap());
                            }
                        };

                        let _ = match lang {
                            Language::C | Language::Cpp | Language::CSharp | Language::Java => {
                                matcher.process_and_drain(
                                    &mut r,
                                    make_c_like_lexer(false, false, args.max_token_length),
                                    write_out_finding,
                                )
                            }
                            Language::Go | Language::Js | Language::Ts | Language::Kotlin => {
                                matcher.process_and_drain(
                                    &mut r,
                                    make_c_like_lexer(true, false, args.max_token_length),
                                    write_out_finding,
                                )
                            }
                            Language::Py => matcher.process_and_drain(
                                &mut r,
                                make_python_like_lexer(false, args.max_token_length),
                                write_out_finding,
                            ),
                            Language::Rust => matcher.process_and_drain(
                                &mut r,
                                make_rust_like_lexer(false, args.max_token_length),
                                write_out_finding,
                            ),
                        };
                    }
                }
            }
        });

        handles.push(handle);
    }

    producer.join().unwrap(); // wait for producer to finish

    for h in handles {
        h.join().unwrap();
    }

    Ok(())
}
