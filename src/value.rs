use std::collections::BTreeMap;
use std::fmt;

use serde::de::Visitor;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde::Deserialize;
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

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("any valid bencode type")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Value, E> {
                Ok(Value::Int(v))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Value, E> {
                Ok(Value::Int(v as i64))
            }

            fn visit_str<E>(self, v: &str) -> Result<Value, E> {
                Ok(Value::Text(v.as_bytes().to_owned()))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_str(v)
            }

            fn visit_string<E>(self, v: String) -> Result<Value, E> {
                Ok(Value::Text(v.into_bytes()))
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E> {
                Ok(Value::Text(v.to_owned()))
            }

            fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_bytes(v)
            }

            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E> {
                Ok(Value::Text(v))
            }

            fn visit_some<D>(self, de: D) -> Result<Self::Value, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                Deserialize::deserialize(de)
            }

            fn visit_seq<A>(self, mut access: A) -> Result<Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut list = Vec::new();
                while let Some(elem) = access.next_element()? {
                    list.push(elem);
                }
                Ok(Value::List(list))
            }

            fn visit_map<A>(self, mut access: A) -> Result<Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut dict = BTreeMap::new();
                while let Some((key, val)) = access.next_entry()? {
                    dict.insert(key, val);
                }
                Ok(Value::Dict(dict))
            }
        }

        de.deserialize_any(ValueVisitor)
    }
}

/// Implements `From<T> for Value` for any numerical type.
macro_rules! impl_value_from_num {
    ($($t:ty),*) => {
        $(
            impl From<$t> for Value {
                fn from(v: $t) -> Value {
                    Value::Int(v as i64)
                }
            }
        )*
    }
}

// We need to skip i64.
impl_value_from_num!(u8, u16, u32, u64, usize, i8, i16, i32, isize);

// We do this manually as to avoid casting `i64 as i64`.
impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Int(v)
    }
}

impl From<&[u8]> for Value {
    fn from(v: &[u8]) -> Self {
        Value::Text(v.to_owned())
    }
}

impl From<Vec<u8>> for Value {
    fn from(v: Vec<u8>) -> Self {
        Value::Text(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::Text(v.as_bytes().to_owned())
    }
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, HashMap};

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

    #[test]
    fn encode_value_dict() {
        let mut map = HashMap::new();
        map.insert("foo".to_string(), Value::Int(1995));
        map.insert("bar".to_string(), Value::Text(b"faz".to_vec()));

        assert_eq!(encode(&map).unwrap(), b"d3:bar3:faz3:fooi1995ee");
    }

    #[test]
    fn decode_value_int() {
        assert_eq!(decode::<Value>(b"i1995e").unwrap(), Value::Int(1995));
    }

    #[test]
    fn decode_value_text() {
        assert_eq!(
            decode::<Value>(b"3:foo").unwrap(),
            Value::Text(b"foo".to_vec())
        );
    }

    #[test]
    fn decode_value_list() {
        assert_eq!(
            decode::<Value>(b"li1995e3:fooe").unwrap(),
            Value::List(vec![Value::Int(1995), Value::Text(b"foo".to_vec())])
        )
    }

    #[test]
    fn decode_value_dict() {
        let mut map = BTreeMap::new();
        map.insert("foo".to_string(), Value::Int(1995));
        map.insert("bar".to_string(), Value::Text(b"faz".to_vec()));
        assert_eq!(
            decode::<Value>(b"d3:bar3:faz3:fooi1995ee").unwrap(),
            Value::Dict(map)
        )
    }
}
