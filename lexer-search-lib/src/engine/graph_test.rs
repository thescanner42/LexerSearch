use std::{collections::HashMap, num::NonZero};

use crate::engine::{
    graph::{GraphBuilder, GraphBuilderNode, GraphTokenVariant, PatternInfo},
    span::SetSpan,
};

#[test]
fn graph_builder_simple_abc() {
    let mut builder: GraphBuilder = Default::default();
    let s = b"a b c";
    let mut c = std::io::Cursor::new(s);
    let lexer = crate::lexer::c_like::Lexer::new(
        NonZero::new(99).unwrap(),
        100,
        false, // backtick_quotes_enabled
        true,  // pattern_enabled
    )
    .unwrap();
    // let
    builder
        .add_pattern(
            &mut c,
            &Default::default(),
            "simple_abc".to_owned(),
            Default::default(),
            &Default::default(),
            lexer,
            NonZero::new(100).unwrap(),
        )
        .unwrap();

    let node1 = GraphBuilderNode {
        edge: HashMap::from([(
            GraphTokenVariant::Captureable(vec![b'a'].into_boxed_slice()),
            SetSpan::from(1, false, false),
        )]),
        ..Default::default()
    };
    let node2 = GraphBuilderNode {
        edge: HashMap::from([(
            GraphTokenVariant::Captureable(vec![b'b'].into_boxed_slice()),
            SetSpan::from(2, false, false),
        )]),
        ..Default::default()
    };
    let node3 = GraphBuilderNode {
        edge: HashMap::from([(
            GraphTokenVariant::Captureable(vec![b'c'].into_boxed_slice()),
            SetSpan::from(3, false, false),
        )]),
        ..Default::default()
    };
    let node4 = GraphBuilderNode {
        full_match: vec![PatternInfo {
            backref_names: Default::default(),
            out: Default::default(),
            name: "simple_abc".to_owned(),
            group: Default::default(),
        }],
        ..Default::default()
    };

    assert_eq!(builder.nodes, vec![node1, node2, node3, node4]);
}

#[test]
fn graph_builder_simple_dedup() {
    let mut builder: GraphBuilder = Default::default();
    let s = b"a b";
    let mut c = std::io::Cursor::new(s);
    let lexer = crate::lexer::c_like::Lexer::new(
        NonZero::new(99).unwrap(),
        100,
        false, // backtick_quotes_enabled
        true,  // pattern_enabled
    )
    .unwrap();
    builder
        .add_pattern(
            &mut c,
            &Default::default(),
            "dedup1".to_owned(),
            Default::default(),
            &Default::default(),
            lexer,
            NonZero::new(100).unwrap(),
        )
        .unwrap();
    let s = b"a c";
    let mut c = std::io::Cursor::new(s);
    let lexer = crate::lexer::c_like::Lexer::new(
        NonZero::new(99).unwrap(),
        100,
        false, // backtick_quotes_enabled
        true,  // pattern_enabled
    )
    .unwrap();
    builder
        .add_pattern(
            &mut c,
            &Default::default(),
            "dedup2".to_owned(),
            Default::default(),
            &Default::default(),
            lexer,
            NonZero::new(100).unwrap(),
        )
        .unwrap();

    let node1 = GraphBuilderNode {
        edge: HashMap::from([(
            GraphTokenVariant::Captureable(vec![b'a'].into_boxed_slice()),
            SetSpan::from(1, false, false),
        )]),
        ..Default::default()
    };
    let node2 = GraphBuilderNode {
        edge: HashMap::from([
            (
                GraphTokenVariant::Captureable(vec![b'b'].into_boxed_slice()),
                SetSpan::from(2, false, false),
            ),
            (
                GraphTokenVariant::Captureable(vec![b'c'].into_boxed_slice()),
                SetSpan::from(3, false, false),
            ),
        ]),
        ..Default::default()
    };
    let node3 = GraphBuilderNode {
        full_match: vec![PatternInfo {
            backref_names: Default::default(),
            out: Default::default(),
            name: "dedup1".to_owned(),
            group: Default::default(),
        }],
        ..Default::default()
    };
    let node4 = GraphBuilderNode {
        full_match: vec![PatternInfo {
            backref_names: Default::default(),
            out: Default::default(),
            name: "dedup2".to_owned(),
            group: Default::default(),
        }],
        ..Default::default()
    };

    assert_eq!(builder.nodes, vec![node1, node2, node3, node4]);
}
