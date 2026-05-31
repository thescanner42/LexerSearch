use ahash::AHashSet;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, ffi::OsStr, path::PathBuf};

use crate::engine::{graph::GroupInfo, matcher::FullMatch};

#[derive(
    Debug,
    Deserialize,
    Serialize,
    bincode::Encode,
    bincode::Decode,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
)]
#[serde(deny_unknown_fields)]
pub enum Language {
    #[serde(rename = "c", alias = "c++", alias = "cpp")]
    C,
    #[serde(rename = "c#", alias = "csharp", alias = "cs")]
    CSharp,
    #[serde(rename = "go", alias = "golang")]
    Go,
    #[serde(rename = "java")]
    Java,
    #[serde(rename = "js", alias = "javascript")]
    Js,
    #[serde(rename = "kt", alias = "kotlin")]
    Kotlin,
    #[serde(rename = "py", alias = "python", alias = "python2", alias = "python3")]
    Py,
    #[serde(rename = "rs", alias = "rust")]
    Rust,
    #[serde(rename = "ts", alias = "typescript")]
    Ts,
}

impl Language {
    pub fn from_file_extension(extension: &OsStr) -> Option<Language> {
        match extension.to_ascii_lowercase().to_str() {
            Some(ext) => match ext {
                "c" | "cpp" | "cxx" | "h" | "hpp" => Some(Language::C),
                "cs" => Some(Language::CSharp),
                "go" => Some(Language::Go),
                "java" => Some(Language::Java),
                "javascript" | "js" | "mjs" => Some(Language::Js),
                "kt" | "kts" => Some(Language::Js),
                "py" => Some(Language::Py),
                "rs" => Some(Language::Rust),
                "typescript" | "ts" => Some(Language::Ts),
                _ => None,
            },
            None => None,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PatternsFile {
    /// stored internally as bytes but serialized as strings
    #[serde(
        deserialize_with = "deserialize_vec_bytes",
        serialize_with = "serialize_vec_bytes"
    )]
    pub patterns: Vec<Box<[u8]>>,

    // make this a set just in case of duplicate languages
    pub languages: AHashSet<Language>,

    #[serde(default)]
    pub name: String,

    /// empty indicates no group
    #[serde(default)]
    pub group: GroupInfo,

    /// backref name -> literal value
    #[serde(
        default,
        deserialize_with = "deserialize_map_bytes_bytes",
        serialize_with = "serialize_map_bytes_bytes"
    )]
    pub out: BTreeMap<Box<[u8]>, Box<[u8]>>,

    /// backref name -> transform regex
    #[serde(
        default,
        deserialize_with = "deserialize_map_bytes_string",
        serialize_with = "serialize_map_bytes_string"
    )]
    pub transform: BTreeMap<Box<[u8]>, String>,

    #[serde(
        default,
        deserialize_with = "deserialize_map_bytes_vec_bytes",
        serialize_with = "serialize_map_bytes_vec_bytes"
    )]
    pub templates: BTreeMap<Box<[u8]>, Vec<Box<[u8]>>>,
}

//
// ---------------------
// DESERIALIZATION
// ---------------------
//

fn deserialize_vec_bytes<'de, D>(deserializer: D) -> Result<Vec<Box<[u8]>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let vec_of_strings = Vec::<String>::deserialize(deserializer)?;
    Ok(vec_of_strings
        .into_iter()
        .map(|s| s.into_bytes().into_boxed_slice())
        .collect())
}

fn deserialize_map_bytes_string<'de, D>(
    deserializer: D,
) -> Result<BTreeMap<Box<[u8]>, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let map = BTreeMap::<String, String>::deserialize(deserializer)?;
    Ok(map
        .into_iter()
        .map(|(k, v)| (k.into_bytes().into_boxed_slice(), v))
        .collect())
}

fn deserialize_map_bytes_bytes<'de, D>(
    deserializer: D,
) -> Result<BTreeMap<Box<[u8]>, Box<[u8]>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let map_of_strings = BTreeMap::<String, String>::deserialize(deserializer)?;
    Ok(map_of_strings
        .into_iter()
        .map(|(k, v)| {
            (
                k.into_bytes().into_boxed_slice(),
                v.into_bytes().into_boxed_slice(),
            )
        })
        .collect())
}

fn deserialize_map_bytes_vec_bytes<'de, D>(
    deserializer: D,
) -> Result<BTreeMap<Box<[u8]>, Vec<Box<[u8]>>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let map = BTreeMap::<String, Vec<String>>::deserialize(deserializer)?;

    Ok(map
        .into_iter()
        .map(|(k, values)| {
            (
                k.into_bytes().into_boxed_slice(),
                values
                    .into_iter()
                    .map(|v| v.into_bytes().into_boxed_slice())
                    .collect(),
            )
        })
        .collect())
}

//
// ---------------------
// SERIALIZATION
// ---------------------
//

fn serialize_vec_bytes<S>(value: &Vec<Box<[u8]>>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let vec: Vec<String> = value
        .iter()
        .map(|b| String::from_utf8_lossy(b).into_owned())
        .collect();

    vec.serialize(serializer)
}

fn serialize_map_bytes_string<S>(
    value: &BTreeMap<Box<[u8]>, String>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let map: BTreeMap<String, String> = value
        .iter()
        .map(|(k, v)| (String::from_utf8_lossy(k).into_owned(), v.clone()))
        .collect();

    map.serialize(serializer)
}

fn serialize_map_bytes_bytes<S>(
    value: &BTreeMap<Box<[u8]>, Box<[u8]>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let map: BTreeMap<String, String> = value
        .iter()
        .map(|(k, v)| {
            (
                String::from_utf8_lossy(k).into_owned(),
                String::from_utf8_lossy(v).into_owned(),
            )
        })
        .collect();

    map.serialize(serializer)
}

fn serialize_map_bytes_vec_bytes<S>(
    value: &BTreeMap<Box<[u8]>, Vec<Box<[u8]>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let converted: BTreeMap<String, Vec<String>> = value
        .iter()
        .map(|(k, values)| {
            (
                String::from_utf8_lossy(k).into_owned(),
                values
                    .iter()
                    .map(|v| String::from_utf8_lossy(v).into_owned())
                    .collect(),
            )
        })
        .collect();

    converted.serialize(serializer)
}

// annotate with which file it came from
#[derive(serde::Serialize)]
pub struct FullMatchOut {
    #[serde(skip_serializing_if = "is_empty_path")]
    pub file: PathBuf,
    #[serde(flatten)]
    pub m: FullMatch,
}

fn is_empty_path(p: &std::path::PathBuf) -> bool {
    p.as_os_str().is_empty()
}

pub fn final_postprocess(mut m: FullMatch) -> Option<FullMatch> {
    if m.group.cancel {
        return None;
    }

    // lastly, remove any suppressed vars
    m.captures.retain(|k, _| !k.starts_with(&[b'_']));
    Some(m)
}
