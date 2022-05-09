//! Bencode decoding and deserialization.

use std::str;
use std::str::Utf8Error;
use std::string::FromUtf8Error;

use serde::de::EnumAccess;
use serde::de::IntoDeserializer;
use serde::de::MapAccess;
use serde::de::SeqAccess;
use serde::de::VariantAccess;
use serde::Deserializer;

use super::DICT_START;
use super::INT_START;
use super::LIST_START;
use super::TEXT_DELIM;
use super::TYPE_END;

/// An error that can occur when decoding types from bencode.
///
/// # Variants
///
/// * `EOF` - The decoder reached the end of the source *unexpectedly*.
/// * `Malformed` - You've given the decoder invalid or malformed input.
/// * `Wanted` - The decoder expected a certain type, but found something else.
/// * `Unsupported` - Tried decoding a type that is not supported by the library.
/// * `Deserialize` - A custom serde deserialization error.
/// * `Utf8` - A UTF-8 error straight from the standard library.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// The decoder unexpectedly reached the end of the source.
    EOF,
    /// The decoder was given invalid or malformed input.
    Malformed,
    /// The decoder tried decoding a certain type, but found something else.
    Wanted {
        /// The exact or starting position of the problem type.
        at: usize,
        /// A name or description of the expected type.
        expected: &'static str,
        /// The problem type.
        found: String,
    },
    /// Tried decoding a type not currently supported by the library.
    Unsupported(&'static str),
    /// A serde deserialization error.
    Deserialize(String),
    /// Tried decoding a string that is not valid UTF-8.
    Utf8(Utf8Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Error::EOF => write!(
                f,
                "the decoder unexpectedly reached the end of the source"
            ),
            Error::Malformed => {
                write!(f, "the decoder was given invalid or malformed input")
            }
            Error::Wanted { at, ref expected, ref found } => write!(
                f,
                "expected to decode {} at column {}, but found '{}' instead",
                expected, at, found
            ),
            Error::Unsupported(ref ty) => {
                write!(
                    f,
                    "decoding of type '{}' is not supported by the library",
                    ty
                )
            }
            Error::Deserialize(ref e) => e.fmt(f),
            Error::Utf8(ref e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl serde::de::Error for Error {
    fn custom<T>(e: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error::Deserialize(e.to_string())
    }
}

impl From<Utf8Error> for Error {
    fn from(e: Utf8Error) -> Self {
        Error::Utf8(e)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(e: FromUtf8Error) -> Self {
        Error::Utf8(e.utf8_error())
    }
}

/// A decoder used to decode types from bencode representation.
///
/// When decoding types, you'd want to use the [`decode`](super::decode) function, but you **can** use the decoder directly - though it's important to note that there is currently no real benefit to doing so.
///
/// # Examples
///
/// ```
/// use serde::Deserialize;
/// use bende::de::Decoder;
///
/// let mut de = Decoder::new(b"i1995e");
/// assert_eq!(i32::deserialize(&mut de).unwrap(), 1995);
///
/// ```
#[derive(Debug)]
pub struct Decoder<'de> {
    src: &'de [u8],
    pos: usize,
}

// Constructor and byte iteration methods.
impl<'de> Decoder<'de> {
    /// Constructs a new decoder with the given source.
    #[inline]
    pub fn new(src: &'de [u8]) -> Decoder<'de> {
        Self { src, pos: 0 }
    }

    /// Gets the length of the source.
    #[inline]
    fn len(&self) -> usize {
        self.src.len()
    }

    /// Gets the decoder's current position.
    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }

    /// Peeks the next byte **without consuming it**.
    #[inline]
    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos()).cloned()
    }

    /// Advances the decoder to the next byte, **consuming it**.
    #[inline]
    fn next(&mut self) -> Option<u8> {
        if self.pos() < self.len() {
            // SAFETY: We've established that the position is in bounds.
            let next = unsafe { *self.src.get_unchecked(self.pos()) };
            self.pos += 1;
            Some(next)
        } else {
            None
        }
    }

    /// Advances the decoder's position by **n**.
    #[inline]
    fn advance(&mut self, n: usize) {
        self.pos += n;
    }

    /// Advances the decoder to the next byte **only if** passing it to the given predicate yields `true`.
    ///
    /// # Errors
    ///
    /// * The decoder has reached the end of the source.
    /// * The predicate yields `false`, at which point `Error::Wanted` is returned.
    #[inline]
    fn advance_if<P>(
        &mut self,
        pred: P,
        expected: &'static str,
    ) -> Result<(), Error>
    where
        P: Fn(u8) -> bool,
    {
        match self.peek() {
            Some(next) if pred(next) => {
                self.advance(1);
                Ok(())
            }
            Some(next) => Err(Error::Wanted {
                at: self.pos(),
                expected,
                found: (next as char).to_string(),
            }),
            _ => Err(Error::EOF),
        }
    }
}

// Decoding methods and helpers.
impl<'de> Decoder<'de> {
    /// Decodes an integer from the source.
    ///
    /// # Errors
    ///
    /// * The first byte is not equal to `INT_START`.
    /// * The bytes are not valid digits.
    #[inline]
    fn decode_int(&mut self) -> Result<i64, Error> {
        self.advance_if(|next| next == INT_START, "an integer")?;

        let start = self.pos();
        while let Some(next) = self.next() {
            if next == TYPE_END {
                // We want to exclude the 'TYPE_END' from the slice, so that means its `pos - 1`.
                let text = str::from_utf8(&self.src[start..self.pos() - 1])?;
                return text.parse().map_err(|_| Error::Malformed);
            }
        }
        Err(Error::Malformed)
    }

    /// Decodes an integer from the source without checking if the first byte is equal to `INT_START`.
    #[inline]
    fn decode_int_unchecked(&mut self) -> Result<i64, Error> {
        // Skip the integer's denotation.
        self.advance(1);

        let start = self.pos();
        while let Some(next) = self.next() {
            if next == TYPE_END {
                // We want to exclude the 'TYPE_END' from the slice, so that means its `pos - 1`.
                let text = str::from_utf8(&self.src[start..self.pos() - 1])?;
                return text.parse().map_err(|_| Error::Malformed);
            }
        }
        Err(Error::Malformed)
    }

    /// Decodes the length of a byte array.
    #[inline]
    fn decode_len(&mut self) -> Result<usize, Error> {
        let start = self.pos();
        while let Some(next) = self.next() {
            if next == TEXT_DELIM {
                // We want to exclude the 'TEXT_DELIM' from the slice, so that means its `pos - 1`.
                let text = str::from_utf8(&self.src[start..self.pos() - 1])?;
                return text.parse().map_err(|_| Error::Wanted {
                    at: start,
                    expected: "a byte array",
                    found: text.to_owned(),
                });
            }
        }
        Err(Error::Malformed)
    }

    /// Decodes a byte array that may or **may not** be valid UTF-8.
    #[inline]
    fn decode_bytes(&mut self) -> Result<&'de [u8], Error> {
        let len = self.decode_len()?;

        // We can avoid iterating over the bytes **entirely** by simply advancing the decoder's position by `len`.
        let start = self.pos();
        self.advance(len);
        let end = self.pos();

        // Less than or equal to because we're using an **exclusive** range.
        if end <= self.len() {
            Ok(&self.src[start..end])
        } else {
            Err(Error::EOF)
        }
    }

    /// Decodes a boolean from the source.
    #[inline]
    fn decode_bool(&mut self) -> Result<bool, Error> {
        // If an error occurs, we're going to need the position before we decode the integer.
        let start = self.pos();
        match self.decode_int()? {
            0 => Ok(false),
            1 => Ok(true),
            found => Err(Error::Wanted {
                at: start,
                expected: "a boolean",
                found: found.to_string(),
            }),
        }
    }
}

impl<'a, 'de> Deserializer<'de> for &'a mut Decoder<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.peek() {
            // We use `decode_int_unchecked` because there is no need to double check if we're working with an integer.
            Some(INT_START) => visitor.visit_i64(self.decode_int_unchecked()?),
            Some(LIST_START) => {
                // Skip over the 'LIST_START'.
                self.advance(1);
                visitor.visit_seq(SeqDecoder::new(self))
            }
            Some(DICT_START) => {
                // Skip over the 'DICT_START'.
                self.advance(1);
                visitor.visit_map(MapDecoder::new(self))
            }
            Some(_) => self.deserialize_bytes(visitor),
            _ => Err(Error::EOF),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_bool(self.decode_bool()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    // All numeric types (except for floats) should be deserialized as an `i64`.
    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i64(self.decode_int()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(Error::Unsupported("f32"))
    }

    fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(Error::Unsupported("f64"))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        // Get our current position before we decode anything.
        let at = self.pos();

        let s: &str = str::from_utf8(&self.decode_bytes()?)?;
        let count = s.chars().count();

        match count {
            1 => visitor.visit_char(s.chars().next().unwrap()),
            _ => Err(Error::Wanted {
                at,
                expected: "a character",
                found: s.to_owned(),
            }),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        // The borrow checker complains if we don't explicitly say that the string lives for 'de.
        let text: &'de str = str::from_utf8(&self.decode_bytes()?)?;
        visitor.visit_borrowed_str(text)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let text: String = String::from_utf8(self.decode_bytes()?.to_vec())?;
        visitor.visit_string(text)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_bytes(&self.decode_bytes()?)
    }

    fn deserialize_byte_buf<V>(
        self,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_byte_buf(self.decode_bytes()?.to_vec())
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        // Get the decoder's position before we decode the name.
        let at = self.pos();

        let found = self.decode_bytes()?;
        if found != name.as_bytes() {
            Err(Error::Wanted {
                at,
                expected: name,
                found: String::from_utf8(found.to_vec())?,
            })
        } else {
            visitor.visit_unit()
        }
    }

    fn deserialize_newtype_struct<V>(
        self,
        _: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // Sequences and tuple types are deserialized as a sequence.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.advance_if(|next| next == LIST_START, "a list of values")?;
        visitor.visit_seq(SeqDecoder::new(self))
    }

    fn deserialize_tuple<V>(
        self,
        _: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _: &'static str,
        _: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Structures are deserialized as a map (or dictionary).
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.advance_if(|next| next == DICT_START, "a dictionary")?;
        visitor.visit_map(MapDecoder::new(self))
    }

    fn deserialize_struct<V>(
        self,
        _: &'static str,
        _: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _: &'static str,
        _: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.peek() == Some(DICT_START) {
            // Skip over the outer dictionary's start denotation.
            self.advance(1);

            let val = visitor.visit_enum(&mut *self)?;

            // Skip over the outer dictionary's end denotation.
            self.advance_if(
                |next| next == TYPE_END,
                "the end of a dictionary",
            )?;
            Ok(val)
        } else {
            visitor.visit_enum(
                str::from_utf8(self.decode_bytes()?)?.into_deserializer(),
            )
        }
    }

    fn deserialize_identifier<V>(
        self,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(
        self,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

impl<'a, 'de> VariantAccess<'de> for &'a mut Decoder<'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        seed.deserialize(self)
    }

    fn tuple_variant<V>(
        self,
        _: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn struct_variant<V>(
        self,
        _: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }
}

impl<'a, 'de> EnumAccess<'de> for &'a mut Decoder<'de> {
    type Error = Error;

    type Variant = Self;

    fn variant_seed<V>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        Ok((seed.deserialize(&mut *self)?, self))
    }
}

/// A decoder used to decode a sequence of types.
#[derive(Debug)]
struct SeqDecoder<'a, 'de: 'a> {
    de: &'a mut Decoder<'de>,
}

impl<'a, 'de> SeqDecoder<'a, 'de> {
    /// Constructs a new sequence decoder.
    #[inline]
    fn new(de: &'a mut Decoder<'de>) -> SeqDecoder<'a, 'de> {
        Self { de }
    }
}

impl<'a, 'de> SeqAccess<'de> for SeqDecoder<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.de.peek() {
            Some(TYPE_END) => {
                // Exclude the 'TYPE_END' for the next iteration.
                self.de.advance(1);
                Ok(None)
            }
            Some(_) => seed.deserialize(&mut *self.de).map(Some),
            _ => Err(Error::EOF),
        }
    }
}

/// A decoder that can decode a key-value object.
#[derive(Debug)]
struct MapDecoder<'a, 'de: 'a> {
    de: &'a mut Decoder<'de>,
}

impl<'a, 'de> MapDecoder<'a, 'de> {
    /// Constructs a new dictionary decoder.
    #[inline]
    fn new(de: &'a mut Decoder<'de>) -> MapDecoder<'a, 'de> {
        Self { de }
    }
}

impl<'a, 'de> MapAccess<'de> for MapDecoder<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        match self.de.peek() {
            Some(TYPE_END) => {
                // Exclude the 'TYPE_END' for the next iteration.
                self.de.advance(1);
                Ok(None)
            }
            Some(b'0'..=b'9') => seed.deserialize(&mut *self.de).map(Some),
            Some(_) => Err(Error::Malformed),
            _ => Err(Error::EOF),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use serde::Deserialize;
    use serde_bytes::ByteBuf;
    use serde_bytes::Bytes;

    use super::Decoder;
    use super::Error;

    /// Asserts that the result of decoding the encoded bytes is equal to the given value.
    macro_rules! test_decode {
        ($enc:expr, $res:expr) => {
            let mut de = Decoder::new($enc);
            assert_eq!(Deserialize::deserialize(&mut de), $res);
        };

        ($t:ident, $enc:expr, $res:expr) => {
            let mut de = Decoder::new($enc);
            assert_eq!($t::deserialize(&mut de), $res);
        };
    }

    #[test]
    fn decode_int_unsigned() {
        test_decode!(u8, b"i255e", Ok(255));
        test_decode!(u16, b"i255e", Ok(255));
        test_decode!(u32, b"i255e", Ok(255));
        test_decode!(usize, b"i255e", Ok(255));

        test_decode!(i8, b"i127e", Ok(127));
        test_decode!(i16, b"i127e", Ok(127));
        test_decode!(i32, b"i127e", Ok(127));
        test_decode!(isize, b"i127e", Ok(127));
    }

    #[test]
    fn decode_int_signed() {
        test_decode!(i8, b"i-127e", Ok(-127));
        test_decode!(i16, b"i-127e", Ok(-127));
        test_decode!(i32, b"i-127e", Ok(-127));
        test_decode!(isize, b"i-127e", Ok(-127));
    }

    #[test]
    fn decode_int_zero() {
        test_decode!(b"i0e", Ok(0));
    }

    #[test]
    fn decode_int_wrong_tag() {
        test_decode!(
            i32,
            b"e1995e",
            Err(Error::Wanted {
                at: 0,
                expected: "an integer",
                found: "e".to_string()
            })
        );
    }

    #[test]
    fn decode_int_invalid_digits() {
        test_decode!(i32, b"i199xe", Err(Error::Malformed));
    }

    #[test]
    fn decode_int_unchecked() {
        let mut de = Decoder::new(b"i1995e");
        assert_eq!(de.decode_int_unchecked(), Ok(1995));

        let mut de = Decoder::new(b"s1995e");
        assert_eq!(de.decode_int_unchecked(), Ok(1995))
    }

    #[test]
    fn decode_float_err() {
        test_decode!(f32, b"i1995e", Err(Error::Unsupported("f32")));
        test_decode!(f64, b"i1995e", Err(Error::Unsupported("f64")));
    }

    #[test]
    fn decode_char_ok() {
        test_decode!(b"1:a", Ok('a'));
    }

    #[test]
    fn decode_char_err() {
        test_decode!(
            char,
            b"3:foo",
            Err(Error::Wanted {
                at: 0,
                expected: "a character",
                found: "foo".to_string()
            })
        );
    }

    #[test]
    fn decode_len_ok() {
        let mut de = Decoder::new(b"13:x");
        assert_eq!(de.decode_len(), Ok(13));
        assert_eq!(de.next(), Some(b'x'));
    }

    #[test]
    fn decode_len_err() {
        let mut de = Decoder::new(b"13x:");
        assert_eq!(
            de.decode_len(),
            Err(Error::Wanted {
                at: 0,
                expected: "a byte array",
                found: "13x".to_string()
            })
        )
    }

    #[test]
    fn decode_bytes_ok() {
        test_decode!(b"3:foo", Ok(Bytes::new(b"foo")));
    }

    #[test]
    fn decode_bytes_empty() {
        test_decode!(b"0:", Ok(Bytes::new(b"")));
    }

    #[test]
    fn decode_bytes_err() {
        test_decode!(ByteBuf, b"4:foo", Err(Error::EOF));
    }

    #[test]
    fn decode_str_ok() {
        test_decode!(b"3:foo", Ok("foo"));
    }

    #[test]
    fn decode_bool_ok() {
        test_decode!(b"i0e", Ok(false));
        test_decode!(b"i1e", Ok(true));
    }

    #[test]
    fn decode_bool_err() {
        test_decode!(
            bool,
            b"i2e",
            Err(Error::Wanted {
                at: 0,
                expected: "a boolean",
                found: "2".to_string()
            })
        );
    }

    #[test]
    fn deserialize_some() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Person {
            name: Option<String>,
            age: u8,
        }
        test_decode!(
            b"d4:name5:Jerry3:agei50ee",
            Ok(Person { name: Some("Jerry".to_string()), age: 50 })
        );
    }

    #[test]
    fn deserialize_none() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Person {
            name: Option<String>,
            age: u8,
        }
        test_decode!(b"d3:agei50ee", Ok(Person { name: None, age: 50 }));
    }

    #[test]
    fn deserialize_unit_struct_ok() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Unit;
        test_decode!(b"4:Unit", Ok(Unit));
    }

    #[test]
    fn deserialize_unit_struct_err() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Unit;
        test_decode!(
            Unit,
            b"3:Foo",
            Err(Error::Wanted {
                at: 0,
                expected: "Unit",
                found: "Foo".to_string()
            })
        );
    }

    #[test]
    fn deserialize_newtype_struct() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Foo(i32);
        test_decode!(b"i1995e", Ok(Foo(1995)));
    }

    #[test]
    fn deserialize_map_ok() {
        let mut map = HashMap::new();
        map.insert("foo", "bar");

        test_decode!(b"d3:foo3:bare", Ok(map));
    }

    #[test]
    fn deserialize_map_err() {
        // Workaround to supply the macro with an explicit type.
        type BadMap = HashMap<i32, String>;

        test_decode!(BadMap, b"di1995e3:fooe", Err(Error::Malformed));
    }

    #[test]
    fn deserialize_unit_variant() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Enum {
            Foo,
            Bar,
            Baz,
        }
        test_decode!(b"3:Foo", Ok(Enum::Foo));
        test_decode!(b"3:Bar", Ok(Enum::Bar));
        test_decode!(b"3:Baz", Ok(Enum::Baz));
    }

    #[test]
    fn deserialize_newtype_variant() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Enum {
            Foo(i32),
            Bar(String),
            Baz(bool),
        }
        test_decode!(b"d3:Fooi50ee", Ok(Enum::Foo(50)));
        test_decode!(b"d3:Bar5:helloe", Ok(Enum::Bar("hello".to_string())));
        test_decode!(b"d3:Bazi1ee", Ok(Enum::Baz(true)));
    }

    #[test]
    fn deserialize_tuple_variant() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Enum {
            Foo(i32, i32),
            Bar(char, bool),
            Baz(String, u8),
        }
        test_decode!(b"d3:Fooli50ei90eee", Ok(Enum::Foo(50, 90)));
        test_decode!(b"d3:Barl1:ai1eee", Ok(Enum::Bar('a', true)));
        test_decode!(b"d3:Bazl3:fooi255eee", Ok(Enum::Baz("foo".into(), 255)));
    }

    #[test]
    fn deserialize_struct_variant() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Enum {
            Foo { a: char, b: char },
        }
        test_decode!(
            b"d3:Food1:a1:z1:b1:yee",
            Ok(Enum::Foo { a: 'z', b: 'y' })
        );
    }

    #[test]
    fn deserialize_simple_struct() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct Person {
            name: String,
            age: u8,
            is_employed: bool,
            #[serde(with = "serde_bytes")]
            signature: Vec<u8>,
        }
        test_decode!(
            b"d4:name11:Jerry Smith3:agei50e11:is_employedi0e9:signature6:jsmithe",
            Ok(
                Person {
                    name: "Jerry Smith".to_string(),
                    age: 50,
                    is_employed: false,
                    signature: b"jsmith".to_vec(),
                }
            )
        );
    }
}
