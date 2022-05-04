//! Bencode encoding and serialization.

use std::collections::BTreeMap;
use std::io::Error as IoError;
use std::io::Write;

use serde::ser::Impossible;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;
use serde::ser::SerializeStructVariant;
use serde::ser::SerializeTuple;
use serde::ser::SerializeTupleStruct;
use serde::ser::SerializeTupleVariant;
use serde::Serialize;
use serde::Serializer;

use super::DICT_START;
use super::INT_START;
use super::LIST_START;
use super::TEXT_DELIM;
use super::TYPE_END;

/// An error that can occur when encoding types to bencode.
///
/// # Variants
///
/// * `Io` - An I/O error from the standard library.
/// * `InvalidKeyType` - When you try encoding a map with keys that are not of type string.
/// * `KeyWithNoValue` - When you try encoding a map entry's key without a value.
/// * `ValueWithNoKey` - When you try encoding a map entry's value without a key.
/// * `Unsupported` - When you try encoding a type that is not currently supported by the library.
/// * `Serialize` - A custom serde serialization error.
#[derive(Debug)]
pub enum Error {
    /// A standard I/O error.
    Io(IoError),
    /// The encoder can only encode maps with keys that are of type string.
    InvalidKeyType,
    /// Tried encoding a map entry's key without a value.
    KeyWithNoValue,
    /// Tried encoding a map entry's value without a key.
    ValueWithNoKey,
    /// Tried encoding a type that is not currently supported by the library.
    Unsupported(&'static str),
    /// A serde serialization error.
    Serialize(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Error::Io(ref e) => e.fmt(f),
            Error::InvalidKeyType => write!(
                f,
                "encoder can only encode keys that are of type string"
            ),
            Error::KeyWithNoValue => {
                write!(f, "tried encoding a map entry's key without a value")
            }
            Error::ValueWithNoKey => {
                write!(f, "tried encoding a map entry's value without a key")
            }
            Error::Unsupported(ref ty) => {
                write!(
                    f,
                    "encoding of type '{}' is not supported by the library",
                    ty
                )
            }
            Error::Serialize(ref e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error::Serialize(msg.to_string())
    }
}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self {
        Error::Io(e)
    }
}

/// An encoder used to encode types to bencode representation.
///
/// When encoding types, you'd typically use the [`encode`](super::encode) function, but you can use this when you want more control over the buffer used to write values to.
///
/// # Examples
///
/// ```
/// use serde::Serialize;
/// use bende::en::Encoder;
///
/// let x = 1995;
///
/// // Give it anything that implements 'Write'.
/// let mut en = Encoder::new(vec![]);
/// x.serialize(&mut en).unwrap();
///
/// assert_eq!(en.into_inner(), b"i1995e");
/// ```
#[derive(Debug)]
pub struct Encoder<W> {
    buf: W,
}

impl<W: Write> Encoder<W> {
    /// Constructs a new encoder with the given buffer.
    ///
    /// The encoder is generic over its buffer, and accepts any type that implements [`Write`].
    #[inline]
    pub fn new(buf: W) -> Encoder<W> {
        Self { buf }
    }

    /// Consumes and returns the encoder's underlying buffer.
    #[inline]
    pub fn into_inner(self) -> W {
        self.buf
    }

    /// Writes a single byte into the buffer.
    #[inline]
    fn tag(&mut self, byte: u8) -> Result<(), Error> {
        self.buf.write_all(&[byte]).map_err(Into::into)
    }

    /// Writes all the given bytes into the buffer.
    #[inline]
    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        self.buf.write_all(bytes).map_err(Into::into)
    }

    /// Encodes an integer into the buffer.
    #[inline]
    fn encode_int(&mut self, v: i64) -> Result<(), Error> {
        self.tag(INT_START)?;
        self.write(v.to_string().as_bytes())?;
        self.tag(TYPE_END)
    }

    /// Encodes a byte array into the buffer.
    #[inline]
    fn encode_bytes(&mut self, bytes: &[u8]) -> Result<(), Error> {
        self.write(bytes.len().to_string().as_bytes())?;
        self.tag(TEXT_DELIM)?;
        self.write(bytes)
    }
}

impl<'a, W: Write> Serializer for &'a mut Encoder<W> {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = SeqEncoder<'a, W>;

    type SerializeTuple = SeqEncoder<'a, W>;

    type SerializeTupleStruct = SeqEncoder<'a, W>;

    type SerializeTupleVariant = SeqEncoder<'a, W>;

    type SerializeMap = MapEncoder<'a, W>;

    type SerializeStruct = MapEncoder<'a, W>;

    type SerializeStructVariant = MapEncoder<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.encode_int(v as i64)
    }

    fn serialize_f32(self, _: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::Unsupported("f32"))
    }

    fn serialize_f64(self, _: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::Unsupported("f64"))
    }

    fn serialize_char(self, _: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::Unsupported("char"))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.encode_bytes(v.as_bytes())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.encode_bytes(v)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::Unsupported("None"))
    }

    fn serialize_some<T: ?Sized>(self, v: &T) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        v.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }

    fn serialize_unit_struct(
        self,
        name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(name)
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _: &'static str,
        v: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        v.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Ok(())
    }

    fn serialize_seq(
        self,
        _: Option<usize>,
    ) -> Result<Self::SerializeSeq, Self::Error> {
        self.tag(LIST_START)?;
        Ok(SeqEncoder::new(self))
    }

    fn serialize_tuple(
        self,
        _: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        self.tag(LIST_START)?;
        Ok(SeqEncoder::new(self))
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.tag(LIST_START)?;
        Ok(SeqEncoder::new(self))
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.tag(LIST_START)?;
        Ok(SeqEncoder::new(self))
    }

    fn serialize_map(
        self,
        _: Option<usize>,
    ) -> Result<Self::SerializeMap, Self::Error> {
        self.tag(DICT_START)?;
        Ok(MapEncoder::new(self))
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.tag(DICT_START)?;
        Ok(MapEncoder::new(self))
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.tag(DICT_START)?;
        Ok(MapEncoder::new(self))
    }
}

/// An encoder used to encode the values in a sequence.
///
/// **Note** that this type cannot be constructed from outside this crate, and is only public because of the way the library's serialization is implemented.
#[derive(Debug)]
pub struct SeqEncoder<'a, W> {
    en: &'a mut Encoder<W>,
}

impl<'a, W> SeqEncoder<'a, W> {
    /// Constructs a new sequence encoder.
    #[inline]
    fn new(en: &'a mut Encoder<W>) -> SeqEncoder<'a, W> {
        Self { en }
    }
}

impl<'a, W: Write> SerializeSeq for SeqEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_element<T: ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeTuple for SeqEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_element<T: ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeTupleStruct for SeqEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeTupleVariant for SeqEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

/// An encoder used to store and **sort** map or struct entries **before** encoding them.
///
/// **Note** that this type cannot be constructed from outside this crate, and is only public because of the way the library's serialization is implemented.
#[derive(Debug)]
pub struct MapEncoder<'a, W> {
    encoder: &'a mut Encoder<W>,
    entries: BTreeMap<Vec<u8>, Vec<u8>>,
    current_key: Option<Vec<u8>>,
}

impl<'a, W> MapEncoder<'a, W> {
    /// Constructs a new map encoder.
    #[inline]
    fn new(encoder: &'a mut Encoder<W>) -> MapEncoder<'a, W> {
        Self { encoder, entries: BTreeMap::new(), current_key: None }
    }
}

impl<'a, W: Write> SerializeMap for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        if self.current_key.is_some() {
            return Err(Error::KeyWithNoValue);
        }

        let mut parent = Encoder::new(vec![]);
        let en = KeyEncoder::new(&mut parent);
        key.serialize(en)?;

        self.current_key = Some(parent.into_inner());
        Ok(())
    }

    fn serialize_value<T: ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        // We don't insert serialized keys into the BTreeMap, otherwise the keys will be sorted by their length first, eg: `1:z` will come before `2:aa`.
        let key = self.current_key.take().ok_or(Error::ValueWithNoKey)?;
        let val = super::encode(&value)?;

        self.entries.insert(key, val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        for (key, val) in self.entries {
            // We need to explicitly use `serialize_bytes` for the keys, otherwise it will be serialized as a list of integers, eg: `li4ei9ei0ee`.
            self.encoder.serialize_bytes(&key)?;

            // We simply write the values to the buffer, otherwise we'll be encoding them **twice**, eg: `3:foo` then becomes `5:3:foo`.
            self.encoder.write(&val)?;
        }
        self.encoder.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeStruct for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        val: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        // No need to use the `KeyEncoder` because we know the key is of type string.
        let key = key.as_bytes().to_vec();
        let val = super::encode(&val)?;

        self.entries.insert(key, val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        for (key, val) in self.entries {
            self.encoder.serialize_bytes(&key)?;
            self.encoder.write(&val)?;
        }
        self.encoder.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeStructVariant for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        val: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let key = key.as_bytes().to_vec();
        let val = super::encode(&val)?;

        self.entries.insert(key, val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        for (key, val) in self.entries {
            self.encoder.serialize_bytes(&key)?;
            self.encoder.write(&val)?;
        }
        self.encoder.tag(TYPE_END)
    }
}

/// An encoder exclusively used to ensure that map keys are of type string before encoding them.
#[derive(Debug)]
struct KeyEncoder<'a, W> {
    en: &'a mut Encoder<W>,
}

impl<'a, W> KeyEncoder<'a, W> {
    /// Constructs a new key encoder.
    #[inline]
    fn new(en: &'a mut Encoder<W>) -> KeyEncoder<'a, W> {
        Self { en }
    }
}

impl<'a, W: Write> Serializer for KeyEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = Impossible<(), Error>;

    type SerializeTuple = Impossible<(), Error>;

    type SerializeTupleStruct = Impossible<(), Error>;

    type SerializeTupleVariant = Impossible<(), Error>;

    type SerializeMap = Impossible<(), Error>;

    type SerializeStruct = Impossible<(), Error>;

    type SerializeStructVariant = Impossible<(), Error>;

    fn serialize_bool(self, _: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_i8(self, _: i8) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_i16(self, _: i16) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_i32(self, _: i32) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_i64(self, _: i64) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_u8(self, _: u8) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_u16(self, _: u16) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_u32(self, _: u32) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_u64(self, _: u64) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_f32(self, _: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_f64(self, _: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_char(self, _: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        // The key encoder should just write the raw string to the buffer. Note that this means you would need to serialize it afterwards.
        self.en.write(v.as_bytes())
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_some<T: ?Sized>(self, _: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::InvalidKeyType)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_unit_struct(
        self,
        _: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::InvalidKeyType)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::InvalidKeyType)
    }

    fn serialize_seq(
        self,
        _: Option<usize>,
    ) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_tuple(
        self,
        _: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_map(
        self,
        _: Option<usize>,
    ) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::InvalidKeyType)
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::InvalidKeyType)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use serde::Serialize;

    use super::Encoder;
    use super::KeyEncoder;

    /// Asserts that the result of encoding the value is equal to the given bencoded bytes.
    macro_rules! test_encode {
        ($val:expr, $res:expr) => {
            let mut en = Encoder::new(vec![]);
            $val.serialize(&mut en).unwrap();
            assert_eq!(en.buf, $res);
        };
    }

    #[test]
    fn encode_int_unsigned() {
        test_encode!(255u8, b"i255e");
        test_encode!(255u16, b"i255e");
        test_encode!(255u32, b"i255e");
        test_encode!(255u64, b"i255e");

        test_encode!(127i8, b"i127e");
        test_encode!(127i16, b"i127e");
        test_encode!(127i32, b"i127e");
        test_encode!(127i64, b"i127e");
    }

    #[test]
    fn encode_int_signed() {
        test_encode!(-127i8, b"i-127e");
        test_encode!(-127i16, b"i-127e");
        test_encode!(-127i32, b"i-127e");
        test_encode!(-127i64, b"i-127e");
    }

    #[test]
    fn encode_int_zero() {
        test_encode!(0, b"i0e");
    }

    #[test]
    fn encode_bytes() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_bytes(b"hello").is_ok());
        assert_eq!(en.buf, b"5:hello".to_vec());
    }

    #[test]
    fn encode_bytes_empty() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_bytes(b"").is_ok());
        assert_eq!(en.buf, b"0:".to_vec());
    }

    #[test]
    fn serialize_simple() {
        #[derive(Debug, PartialEq, Serialize)]
        struct Person {
            name: String,
            age: u8,
            is_employed: bool,
            #[serde(with = "serde_bytes")]
            signature: Vec<u8>,
        }

        let jerry = Person {
            name: "Jerry Smith".to_string(),
            age: 50,
            is_employed: false,
            signature: b"jsmith".to_vec(),
        };

        let mut en = Encoder::new(vec![]);
        assert!(jerry.serialize(&mut en).is_ok());
        assert_eq!(
            en.buf,
            b"d3:agei50e11:is_employedi0e4:name11:Jerry Smith9:signature6:jsmithe".to_vec()
        );
    }

    #[test]
    fn serialize_some() {
        #[derive(Debug, PartialEq, Serialize)]
        struct Person {
            name: Option<String>,
            age: u8,
        }

        let jerry = Person { name: Some("Jerry".to_owned()), age: 50 };

        let mut en = Encoder::new(vec![]);
        assert!(jerry.serialize(&mut en).is_ok());
        assert_eq!(en.buf, b"d3:agei50e4:name5:Jerrye".to_vec());
    }

    #[test]
    #[should_panic]
    fn serialize_none_err() {
        #[derive(Debug, PartialEq, Serialize)]
        struct Person {
            name: Option<String>,
            age: u8,
        }

        let jerry = Person { name: None, age: 50 };

        let mut en = Encoder::new(vec![]);
        jerry.serialize(&mut en).unwrap();
    }

    #[test]
    fn serialize_unit_struct() {
        #[derive(Debug, Serialize)]
        struct Unit;

        let mut en = Encoder::new(vec![]);
        Unit.serialize(&mut en).unwrap();
        assert_eq!(en.buf, b"4:Unit".to_vec());
    }

    #[test]
    fn serialize_newtype_struct() {
        #[derive(Debug, Serialize)]
        struct Foo(i32);

        let mut en = Encoder::new(vec![]);
        Foo(1995).serialize(&mut en).unwrap();
        assert_eq!(en.buf, b"i1995e".to_vec());
    }

    #[test]
    fn encode_key_ok() {
        let mut parent = Encoder::new(vec![]);

        let en = KeyEncoder::new(&mut parent);
        assert!("foo".serialize(en).is_ok());

        let en = KeyEncoder::new(&mut parent);
        assert!("foo".to_string().serialize(en).is_ok());
    }

    #[test]
    fn encode_key_err() {
        let mut parent = Encoder::new(vec![]);

        let en = KeyEncoder::new(&mut parent);
        assert!((0i32).serialize(en).is_err());

        let en = KeyEncoder::new(&mut parent);
        assert!((true).serialize(en).is_err());
    }

    #[test]
    fn serialize_map_ok() {
        let mut map = HashMap::new();
        map.insert("foo", "bar");

        let mut en = Encoder::new(vec![]);
        assert!(map.serialize(&mut en).is_ok());
    }

    #[test]
    fn serialize_map_err() {
        let mut map = HashMap::new();
        map.insert(0, "bar");

        let mut en = Encoder::new(vec![]);
        assert!(map.serialize(&mut en).is_err());
    }

    #[test]
    fn serialized_map_is_sorted() {
        let mut map = HashMap::new();
        map.insert("foo", "bar");
        map.insert("baz", "faz");

        let mut en = Encoder::new(vec![]);
        map.serialize(&mut en).unwrap();

        assert_eq!(en.buf, b"d3:baz3:faz3:foo3:bare");
    }

    #[test]
    fn serialized_struct_is_sorted() {
        #[derive(Debug, PartialEq, Serialize)]
        struct Foo {
            c: i32,
            b: i32,
            a: i32,
        }

        let foo = Foo { c: 3, b: 2, a: 1 };

        let mut en = Encoder::new(vec![]);
        foo.serialize(&mut en).unwrap();

        assert_eq!(en.buf, b"d1:ai1e1:bi2e1:ci3ee")
    }
}
