/// A list of bencode values.
pub type List = Vec<Value>;

/// Represents any valid data type that can be encoded/decoded to and from bencode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A 64-bit signed integer.
    Int(i64),
    /// An array of bytes that may or **may not** be valid UTF-8.
    Text(Vec<u8>),
    /// A list of bencode values.
    List(List),
}
