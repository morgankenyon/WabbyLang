# WauxLang

WauxLang - A language that compiles to WASM bytecode.

This is currently an experimental language/compiler targeting WASM as it's runtime.

Compiler is written in F#.

Current goal: get a simple language compiling and running in WASM

Should I use this? No.

Why Waux? What does that word mean? How do you pronounce it?
  * Pronounced like the English work "walk" [1](https://www.collinsdictionary.com/us/dictionary/english/wauk)
  * "A Scots word for wake"
  * Why Waux?
    * Overall, I just needed to call it something.
    * But wanted something starting with Wa- to match Wasm, and it wasn't that bad of a word. 

## Current Examples

Right now I have basic mathematical operations compiling into wasm:

### Adding

```
5 + 2
```

Compiles into:

```wat
(module
  (type $t0 (func (result i32)))
  (func $main (export "main") (type $t0) (result i32)
    (i32.add
      (i32.const 5)
      (i32.const 2))))
```