use std::collections::BTreeMap;

use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde::Serialize;

/// A list of bencode values.
pub type List = Vec<Value>;

/// A **sorted** key-value map with keys that are UTF-8 valid strings.
pub type Dict = BTreeMap<String, Value>;

/// Represents any valid data type that can be encoded/decoded to and from bencode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A 64-bit signed integer.
    Int(i64),
    /// An array of bytes that may or **may not** be valid UTF-8.
    Text(Vec<u8>),
    /// A list of bencode values.
    List(List),
    /// A key-value map with keys that are UTF-8 valid strings.
    Dict(Dict),
}

impl Serialize for Value {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            Value::Int(v) => ser.serialize_i64(v),
            Value::Text(ref v) => ser.serialize_bytes(v),
            Value::List(ref v) => {
                let mut seq = ser.serialize_seq(Some(v.len()))?;
                for elem in v {
                    seq.serialize_element(elem)?;
                }
                seq.end()
            }
            Value::Dict(ref v) => {
                let mut map = ser.serialize_map(Some(v.len()))?;
                for (key, val) in v {
                    map.serialize_entry(key, val)?;
                }
                map.end()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Value;
    use crate::{decode, encode};

    #[test]
    fn encode_value_int() {
        let mut val = Value::Int(1995);
        assert_eq!(encode(&val).unwrap(), b"i1995e");
    }

    #[test]
    fn encode_value_text() {
        let mut val = Value::Text(b"foo".to_vec());
        assert_eq!(encode(&val).unwrap(), b"3:foo");
    }

    #[test]
    fn encode_value_list() {
        let mut val =
            Value::List(vec![Value::Int(1995), Value::Text(b"foo".to_vec())]);
        assert_eq!(encode(&val).unwrap(), b"li1995e3:fooe");
    }
}
