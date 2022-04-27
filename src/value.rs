/// Represents any valid data type that can be encoded/decoded to and from bencode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A 64-bit signed integer.
    Int(i64),
}
