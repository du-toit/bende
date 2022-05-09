//! A bencode encoding/decoding implementation backed by serde.
//!
//! This library exposes two simple functions:
//!
//! * [`decode`] - Which you can use to decode bencoded bytes into a **deserializable** type.
//! * [`encode`] - Which you can use to encode a **serializable** type into bencoded bytes.
//!
//! Additionally, we have the [`Value`] type that represents any valid bencode data type. It also implements [`Serialize`] and [`Deserialize`].
//!
//! You'd also find error types for both encoding and decoding, alongside the [`Encoder`](en::Encoder) and [`Decoder`](de::Decoder) types.

pub mod de;
pub mod en;
pub mod value;

pub use value::Value;

use serde::{Deserialize, Serialize};

/// Denotes the start of an integer - `i`.
const INT_START: u8 = 0x69;

/// Denotes that start of a list - `l`.
const LIST_START: u8 = 0x6C;

/// Denotes the start of a dictionary - `d`.
const DICT_START: u8 = 0x64;

/// A symbol used to separate a byte array's length from its content - `:`.
const TEXT_DELIM: u8 = 0x3A;

/// Denotes the end of a type - `e`.
const TYPE_END: u8 = 0x65;

/// Encodes the given value into bencode representation.
///
/// # Examples
///
/// ```
/// use serde::Serialize;
///
/// #[derive(Debug, PartialEq, Serialize)]
/// struct Person {
///     name: String,
///     age: u8,
///     is_employed: bool,
///     #[serde(with = "serde_bytes")]
///     signature: Vec<u8>,
/// }
///
/// let jerry = Person {
///     name: "Jerry Smith".to_string(),
///     age: 50,
///     is_employed: false,
///     signature: b"jsmith".to_vec(),
/// };
///
/// assert_eq!(
///     bende::encode(&jerry).unwrap(),
///     b"d3:agei50e11:is_employedi0e4:name11:Jerry Smith9:signature6:jsmithe".to_vec()
/// );
///
/// ```
pub fn encode<T>(val: &T) -> Result<Vec<u8>, en::Error>
where
    T: Serialize,
{
    let mut en = en::Encoder::new(vec![]);
    val.serialize(&mut en)?;
    Ok(en.into_inner())
}

/// Decodes a type from the given bencoded bytes.
///
/// # Examples
///
/// ```
/// use serde::Deserialize;
///
/// #[derive(Debug, PartialEq, Deserialize)]
/// struct Person {
///     name: String,
///     age: u8,
///     is_employed: bool,
///     #[serde(with = "serde_bytes")]
///     signature: Vec<u8>,
/// }
///
/// let jerry = Person {
///     name: "Jerry Smith".to_string(),
///     age: 50,
///     is_employed: false,
///     signature: b"jsmith".to_vec(),
/// };
///
/// assert_eq!(
///     bende::decode(b"d4:name11:Jerry Smith3:agei50e11:is_employedi0e9:signature6:jsmithe"),
///     Ok(jerry)
/// );
/// ```
pub fn decode<'de, T>(bytes: &'de [u8]) -> Result<T, de::Error>
where
    T: Deserialize<'de>,
{
    let mut de = de::Decoder::new(bytes);
    T::deserialize(&mut de)
}

#[cfg(test)]
mod test {
    use serde::{Deserialize, Serialize};

    use super::decode;
    use super::encode;

    /// Asserts that passing the encoded value's bytes to the decoder will yield the same value.
    macro_rules! test_bende {
        ($t:ident, $val:expr) => {
            let bytes = encode(&$val).unwrap();
            assert_eq!(decode::<$t>(&bytes), Ok($val));
        };
    }

    #[test]
    fn encode_and_decode_simple_struct() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Person {
            name: String,
            age: u8,
            is_employed: bool,
        }
        test_bende!(
            Person,
            Person {
                name: "Jerry Smith".to_string(),
                age: 50,
                is_employed: false,
            }
        );
    }

    #[test]
    fn encode_and_decode_nested_enum() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Player {
            name: String,
            class: Class,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        enum Class {
            Wizard,
            Fighter,
        }

        test_bende!(
            Player,
            Player { name: "Gandalf".into(), class: Class::Wizard }
        );
        test_bende!(
            Player,
            Player { name: "Gimly".into(), class: Class::Fighter }
        );
    }

    #[test]
    fn encode_and_decode_nested_struct() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Armory {
            weapons: Vec<Weapon>,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Weapon {
            name: String,
            stats: Stats,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Stats {
            health: u8,
            damage: u8,
        }

        test_bende!(
            Armory,
            Armory {
                weapons: vec![
                    Weapon {
                        name: "Sword".to_string(),
                        stats: Stats { health: 64, damage: 27 },
                    },
                    Weapon {
                        name: "Shield".to_string(),
                        stats: Stats { health: 102, damage: 0 },
                    },
                ],
            }
        );
    }

    #[test]
    fn encode_and_decode_option() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Person {
            name: Option<String>,
            age: Option<u8>,
            is_employed: Option<bool>,
        }
        test_bende!(
            Person,
            Person {
                name: Some("Jerry Smith".to_string()),
                age: Some(50),
                is_employed: Some(false),
            }
        );
    }

    #[test]
    fn encode_and_decode_unit_struct() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Foo;
        test_bende!(Foo, Foo);
    }

    #[test]
    fn encode_and_decode_newtype_struct() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct Foo(i32);
        test_bende!(Foo, Foo(1995));
    }
}
