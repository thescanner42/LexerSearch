use std::{
    fs::File,
    io::BufReader,
    num::{NonZero, NonZeroUsize},
    path::PathBuf,
};

use clap::Parser;

use lexer_search_lib::{
    engine::{
        matcher::{FullMatch, Matcher},
        matchers::{Tries, make_c_like_lexer, make_python_like_lexer, make_rust_like_lexer},
    },
    io::{FullMatchOut, Language},
    lexer::DEFAULT_MAX_TOKEN_LENGTH,
};

#[derive(Parser, Debug)]
#[command(name = "lexer-search")]
#[command(about = "Source code scanner")]
pub struct Args {
    #[cfg(not(feature = "embed-patterns"))]
    pub patterns_path: PathBuf,
    pub scan_path: PathBuf,
    #[arg(default_value_t = 5000)]
    pub max_concurrent_matches: usize,
    #[arg(default_value_t = DEFAULT_MAX_TOKEN_LENGTH)]
    pub max_token_length: NonZeroUsize,
    #[arg(default_value_t = 10.try_into().unwrap())]
    pub group_cap: NonZero<usize>,
}

#[cfg(feature = "embed-patterns")]
// '/' ok even on windows
const EMBEDDED_PATTERNS: &'static [u8] = include_bytes!("../target/lexer-search-embedded-patterns");

fn main() -> Result<(), String> {
    let args = Args::parse();

    #[allow(unused)]
    let mut tries = Tries::default();

    // load rules from cli
    #[cfg(not(feature = "embed-patterns"))]
    {
        tries = Tries::construct_tries(&args.patterns_path, args.max_token_length)?;
    }

    // load rules from embedded patterns
    #[cfg(feature = "embed-patterns")]
    {
        let res = bincode::serde::decode_from_slice(EMBEDDED_PATTERNS, bincode::config::standard())
            .expect("failed to load embedded patterns");
        tries = res.0;
    }

    for entry in walkdir::WalkDir::new(&args.scan_path)
        .into_iter()
        .filter_map(Result::ok)
    {
        if entry.file_type().is_file() {
            let path = entry.path();
            let write_out_finding = |m: FullMatch| {
                println!(
                    "{}",
                    serde_json::to_string(&FullMatchOut {
                        file: path.strip_prefix(&args.scan_path).unwrap(),
                        start: m.start,
                        end: m.end,
                        name: m.name,
                        group: m.group,
                        captures: &m.captures
                    })
                    .unwrap()
                );
            };

            if let Some(ext) = path.extension() {
                match Language::from_file_extension(ext) {
                    Some(lang) => {
                        let file = File::open(path).map_err(|e| e.to_string())?;
                        let mut r = BufReader::new(file);
                        let trie = tries.trie_for_lang(lang);
                        let mut matcher = Matcher::new(
                            trie,
                            args.max_concurrent_matches,
                            args.max_token_length,
                            args.group_cap,
                        );
                        match lang {
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
                        }?;
                    }
                    None => {}
                }
            }
        }
    }

    Ok(())
}
