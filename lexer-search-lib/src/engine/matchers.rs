use std::{num::NonZeroUsize, path::Path};

use crate::{
    engine::{
        graph::{Graph, GraphBuilder},
        template,
    },
    io::Language,
};

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

#[derive(Default, Debug)]
struct GraphsBuilder {
    c_graph: GraphBuilder,
    cpp_graph: GraphBuilder,
    csharp_graph: GraphBuilder,
    go_graph: GraphBuilder,
    java_graph: GraphBuilder,
    js_graph: GraphBuilder,
    kotlin_graph: GraphBuilder,
    python_graph: GraphBuilder,
    rust_graph: GraphBuilder,
    ts_graph: GraphBuilder,
}

impl GraphsBuilder {
    pub fn graph_for_lang_mut(&mut self, lang: Language) -> &mut GraphBuilder {
        match lang {
            Language::C => &mut self.c_graph,
            Language::Cpp => &mut self.cpp_graph,
            Language::CSharp => &mut self.csharp_graph,
            Language::Go => &mut self.go_graph,
            Language::Java => &mut self.java_graph,
            Language::Js => &mut self.js_graph,
            Language::Kotlin => &mut self.kotlin_graph,
            Language::Py => &mut self.python_graph,
            Language::Rust => &mut self.rust_graph,
            Language::Ts => &mut self.ts_graph,
        }
    }
}

#[derive(Debug, serde::Serialize, bincode::Encode, bincode::Decode)]
pub struct Graphs {
    #[serde(skip_serializing_if = "Graph::is_default")]
    c_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    cpp_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    csharp_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    go_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    java_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    js_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    kotlin_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    python_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    rust_graph: Graph,
    #[serde(skip_serializing_if = "Graph::is_default")]
    ts_graph: Graph,
}

impl Graphs {
    pub fn graph_for_lang(&self, lang: Language) -> &Graph {
        match lang {
            Language::C => &self.c_graph,
            Language::Cpp => &self.cpp_graph,
            Language::CSharp => &self.csharp_graph,
            Language::Go => &self.go_graph,
            Language::Java => &self.java_graph,
            Language::Js => &self.js_graph,
            Language::Kotlin => &self.kotlin_graph,
            Language::Py => &self.python_graph,
            Language::Rust => &self.rust_graph,
            Language::Ts => &self.ts_graph,
        }
    }

    pub fn graph_for_lang_mut(&mut self, lang: Language) -> &mut Graph {
        match lang {
            Language::C => &mut self.c_graph,
            Language::Cpp => &mut self.cpp_graph,
            Language::CSharp => &mut self.csharp_graph,
            Language::Go => &mut self.go_graph,
            Language::Java => &mut self.java_graph,
            Language::Js => &mut self.js_graph,
            Language::Kotlin => &mut self.kotlin_graph,
            Language::Py => &mut self.python_graph,
            Language::Rust => &mut self.rust_graph,
            Language::Ts => &mut self.ts_graph,
        }
    }

    pub fn construct_graphs(
        pattern_path: &Path,
        max_token_length: NonZeroUsize,
        max_template_expansions: NonZeroUsize,
    ) -> Result<Self, String> {
        let mut builders = GraphsBuilder::default();

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
                        for unexpanded_pattern in patterns_file.patterns.into_iter() {
                            for pattern in template::expand(
                                &unexpanded_pattern,
                                &patterns_file.templates,
                                max_template_expansions,
                            )? {
                                for lang in patterns_file.languages.iter() {
                                    let mut reader = std::io::Cursor::new(&pattern);
                                    match lang {
                                        Language::C
                                        | Language::Cpp
                                        | Language::CSharp
                                        | Language::Java => {
                                            builders.graph_for_lang_mut(*lang).add_pattern(
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
                                            builders.graph_for_lang_mut(*lang).add_pattern(
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
                                            builders.graph_for_lang_mut(*lang).add_pattern(
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
                                            builders.graph_for_lang_mut(*lang).add_pattern(
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
        }

        Ok(Graphs {
            c_graph: builders.c_graph.build()?,
            cpp_graph: builders.cpp_graph.build()?,
            csharp_graph: builders.csharp_graph.build()?,
            go_graph: builders.go_graph.build()?,
            java_graph: builders.java_graph.build()?,
            js_graph: builders.js_graph.build()?,
            kotlin_graph: builders.kotlin_graph.build()?,
            python_graph: builders.python_graph.build()?,
            rust_graph: builders.rust_graph.build()?,
            ts_graph: builders.ts_graph.build()?,
        })
    }
}
