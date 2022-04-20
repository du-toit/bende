//! Bencode encoding and serialization.

use std::io::Error as IoError;
use std::io::Write;

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
/// * `Unsupported` - When you try encoding a type that is not currently supported by the library.
/// * `Serialize` - A custom serde serialization error.
#[derive(Debug)]
pub enum Error {
    /// A standard I/O error.
    Io(IoError),
    /// Tried encoding a type that is not currently supported by the library.
    Unsupported(&'static str),
    /// A serde serialization error.
    Serialize(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Error::Io(ref e) => e.fmt(f),
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
        _: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(())
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
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Ok(())
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

/// An encoder used to encode key-values in a map or struct.
///
/// **Note** that this type cannot be constructed from outside this crate, and is only public because of the way the library's serialization is implemented.
#[derive(Debug)]
pub struct MapEncoder<'a, W> {
    en: &'a mut Encoder<W>,
}

impl<'a, W> MapEncoder<'a, W> {
    /// Constructs a new map encoder.
    #[inline]
    fn new(en: &'a mut Encoder<W>) -> MapEncoder<'a, W> {
        Self { en }
    }
}

impl<'a, W: Write> SerializeMap for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        key.serialize(&mut *self.en)
    }

    fn serialize_value<T: ?Sized>(
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

impl<'a, W: Write> SerializeStruct for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        key.serialize(&mut *self.en)?;
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

impl<'a, W: Write> SerializeStructVariant for MapEncoder<'a, W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        key.serialize(&mut *self.en)?;
        value.serialize(&mut *self.en)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.en.tag(TYPE_END)
    }
}

#[cfg(test)]
mod test {
    use serde::Serialize;

    use super::Encoder;

    #[test]
    fn encode_int_unsigned() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_int(1995).is_ok());
        assert_eq!(en.buf, b"i1995e".to_vec());
    }

    #[test]
    fn encode_int_signed() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_int(-1995).is_ok());
        assert_eq!(en.buf, b"i-1995e".to_vec());
    }

    #[test]
    fn encode_int_zero() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_int(0).is_ok());
        assert_eq!(en.buf, b"i0e".to_vec());
    }

    #[test]
    fn encode_bytes() {
        let mut en = Encoder::new(vec![]);
        assert!(en.encode_bytes(b"hello").is_ok());
        assert_eq!(en.buf, b"5:hello".to_vec());
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
            b"d4:name11:Jerry Smith3:agei50e11:is_employedi0e9:signature6:jsmithe".to_vec()
        );
    }
}
