use std::{fs::OpenOptions, num::NonZeroUsize, path::PathBuf};

use lexer_search_lib::engine::matchers::Tries;

fn main() -> Result<(), String> {
    if std::env::var_os("CARGO_FEATURE_EMBED_PATTERNS").is_none() {
        return Ok(()); // only if feature
    }

    let patterns_path = std::env::var("LEXERSEARCH_EMBED_PATTERNS").map_err(|e| e.to_string())?;

    let max_token_length = match std::env::var("LEXERSEARCH_MAX_TOKEN_LENGTH") {
        Ok(val) => val.parse::<NonZeroUsize>().map_err(|e| e.to_string())?,
        Err(_) => lexer_search_lib::lexer::DEFAULT_MAX_TOKEN_LENGTH,
    };

    let tries = Tries::construct_tries(std::path::Path::new(&patterns_path), max_token_length)?;

    let mut path = PathBuf::new();
    path.push("target");
    path.push("lexer-search-embedded-patterns");

    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .map_err(|e| e.to_string())?;

    let _size =
        bincode::serde::encode_into_std_write(&tries, &mut file, bincode::config::standard())
            .map_err(|e| e.to_string())?;

    println!("cargo:rerun-if-changed={}", patterns_path);
    Ok(())
}
