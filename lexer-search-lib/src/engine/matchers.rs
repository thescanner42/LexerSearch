use std::{num::NonZeroUsize, path::Path};

use crate::{engine::matcher::Trie, io::Language};

pub fn make_c_like_lexer(
    backtick_strings: bool,
    pattern_enabled: bool,
    max_token_length: NonZeroUsize,
) -> crate::lexer::c_like::Lexer {
    crate::lexer::c_like::Lexer::new(
        max_token_length,
        max_token_length.get() * 10,
        backtick_strings,
        pattern_enabled,
    )
    .unwrap()
}

pub fn make_python_like_lexer(
    pattern_enabled: bool,
    max_token_length: NonZeroUsize,
) -> crate::lexer::python_like::Lexer {
    crate::lexer::python_like::Lexer::new(
        max_token_length,
        max_token_length.get() * 10,
        unsafe { NonZeroUsize::new_unchecked(1000) }, // not bothering with cli
        pattern_enabled,
    )
    .unwrap()
}

pub fn make_rust_like_lexer(
    pattern_enabled: bool,
    max_token_length: NonZeroUsize,
) -> crate::lexer::rust_like::Lexer {
    crate::lexer::rust_like::Lexer::new(
        max_token_length,
        max_token_length.get() * 10,
        pattern_enabled,
    )
    .unwrap()
}

#[derive(Default, Debug, serde::Serialize, serde::Deserialize)]
pub struct Tries {
    c_trie: Trie,
    cpp_trie: Trie,
    csharp_trie: Trie,
    go_trie: Trie,
    java_trie: Trie,
    js_trie: Trie,
    kotlin_trie: Trie,
    python_trie: Trie,
    rust_trie: Trie,
    ts_trie: Trie,
}

impl Tries {
    pub fn trie_for_lang(&self, lang: Language) -> &Trie {
        match lang {
            Language::C => &self.c_trie,
            Language::Cpp => &self.cpp_trie,
            Language::CSharp => &self.csharp_trie,
            Language::Go => &self.go_trie,
            Language::Java => &self.java_trie,
            Language::Js => &self.js_trie,
            Language::Kotlin => &self.kotlin_trie,
            Language::Py => &self.python_trie,
            Language::Rust => &self.rust_trie,
            Language::Ts => &self.ts_trie,
        }
    }

    pub fn trie_for_lang_mut(&mut self, lang: Language) -> &mut Trie {
        match lang {
            Language::C => &mut self.c_trie,
            Language::Cpp => &mut self.cpp_trie,
            Language::CSharp => &mut self.csharp_trie,
            Language::Go => &mut self.go_trie,
            Language::Java => &mut self.java_trie,
            Language::Js => &mut self.js_trie,
            Language::Kotlin => &mut self.kotlin_trie,
            Language::Py => &mut self.python_trie,
            Language::Rust => &mut self.rust_trie,
            Language::Ts => &mut self.ts_trie,
        }
    }

    pub fn construct_tries(
        pattern_path: &Path,
        max_token_length: NonZeroUsize,
    ) -> Result<Self, String> {
        let mut tries = Tries::default();

        for entry in walkdir::WalkDir::new(pattern_path)
            .into_iter()
            .filter_map(Result::ok)
        {
            if entry.file_type().is_file() {
                let path = entry.path();
                if let Some(ext) = path.extension() {
                    if ext == "yml" || ext == "yaml" {
                        use crate::io::PatternsFile;

                        let txt = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
                        let patterns_file: PatternsFile =
                            serde_yml::from_str(&txt).map_err(|e| {
                                format!("YAML load {}: {}", path.display(), e.to_string())
                            })?;
                        for pattern in patterns_file.patterns.iter() {
                            for lang in patterns_file.languages.iter() {
                                let mut reader = std::io::Cursor::new(pattern);
                                match lang {
                                    Language::C
                                    | Language::Cpp
                                    | Language::CSharp
                                    | Language::Java => {
                                        tries.trie_for_lang_mut(*lang).add_pattern(
                                            &mut reader,
                                            &patterns_file.out,
                                            patterns_file.name.clone(),
                                            patterns_file.group.clone(),
                                            &patterns_file.transform,
                                            make_c_like_lexer(false, true, max_token_length),
                                            max_token_length,
                                        )?;
                                    }
                                    Language::Go
                                    | Language::Js
                                    | Language::Ts
                                    | Language::Kotlin => {
                                        tries.trie_for_lang_mut(*lang).add_pattern(
                                            &mut reader,
                                            &patterns_file.out,
                                            patterns_file.name.clone(),
                                            patterns_file.group.clone(),
                                            &patterns_file.transform,
                                            make_c_like_lexer(true, true, max_token_length),
                                            max_token_length,
                                        )?;
                                    }
                                    Language::Py => {
                                        tries.trie_for_lang_mut(*lang).add_pattern(
                                            &mut reader,
                                            &patterns_file.out,
                                            patterns_file.name.clone(),
                                            patterns_file.group.clone(),
                                            &patterns_file.transform,
                                            make_python_like_lexer(true, max_token_length),
                                            max_token_length,
                                        )?;
                                    }
                                    Language::Rust => {
                                        tries.trie_for_lang_mut(*lang).add_pattern(
                                            &mut reader,
                                            &patterns_file.out,
                                            patterns_file.name.clone(),
                                            patterns_file.group.clone(),
                                            &patterns_file.transform,
                                            make_rust_like_lexer(true, max_token_length),
                                            max_token_length,
                                        )?;
                                    }
                                };
                            }
                        }
                    }
                }
            }
        }

        Ok(tries)
    }
}
