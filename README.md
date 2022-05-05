<br />
<div align="center">
    <h1>Bende</h1>
    <p>A rust bencode encoding/decoding implementation backed by serde.</p>
</div>

## About

This is one of a few bencode implementations available for rust. Though there are alternatives (see below), implementing bencode is both fun and a good learning experience. It also never hurts to have one more alternative.

### Alternatives

* [bendy](https://github.com/P3KI/bendy) by P3KI.
* [serde-bencode](https://github.com/toby/serde-bencode) by Toby Padilla.
* [rust-bencode](https://github.com/arjantop/rust-bencode) by Arjan Topolovec.

[There are more](https://crates.io/search?q=bencode&sort=downloads), but some are no longer maintained.

## Usage

Add the library as a dependency to [Cargo.toml](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html)

```toml
[dependencies]
bende = "0.5.4"
serde = { version = "1", features = ["derive"] }
```

### Example

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    is_employed: bool,
}

let jerry = Person {
    name: "Jerry Smith".to_string(),
    age: 50,
    is_employed: false,
};

let bytes = bende::encode(&jerry).unwrap();
assert_eq!(bende::decode::<Person>(&bytes).unwrap(), jerry);
```

## Unsupported Types

The types that are **not supported** are:

* `f32`
* `f64`

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Notes

* Both variants of `Option<_>` (Some and None) are supported by the decoder, **but** the encoder only supports `Some`.
* Keys in a key-value object must be strings, otherwise an error is returned.
* Map and struct entries are sorted lexicographically by their key **before** they are encoded.
* If you run into trouble encoding/decoding raw bytes, eg: `&[u8]` or `Vec<u8>` then use [this crate](https://crates.io/crates/serde_bytes).
* The codebase is relatively small (~2000 lines), easily digestible and filled with comments. If you're a first timer, you'll have a jolly time making your first contribution.
