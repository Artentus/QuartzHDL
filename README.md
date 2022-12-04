## About
Quartz is a hardware description language with Rust-like syntax.  
Also borrowed from Rust is the particularily strict type system, and as such Quartz does not perform any implicit conversions.  
Another goal is also to provide high quality compiler messages similar to those of the Rust compiler.

Currently Quartz compiles into (a subset of) SystemVerilog and can therefore be used with any tooling that supports SV.  
Notice that the project is still in its early stages and therefore the produced SV code may not be valid or semantically correct.

For an example of Quartz syntax and grammar see `test.qrz`.

## Building
Building the Quartz compiler requires a nightly Rust compiler.  
The recommended way of building is using the Rust package manager `cargo`:
```
cargo build --release
```
