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

## Installing from Source

TBD

## Running

TBD

## Current Examples

Right now I have basic mathematical operations compiling into wasm. For the examples below, I'm converting the compiled wasm to wat for visual purposes. But the output of the compiler is wasm byte code.

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

### Subtraction

`10 - 2`

Compiles into:

```wat
(module
  (type $t0 (func (result i32)))
  (func $main (export "main") (type $t0) (result i32)
    (i32.sub
      (i32.const 10)
      (i32.const 2))))
```

### Operator Precedence

It also currently handles operator precedence:

`10 + 10 / 5 * 2 - 1`

Compiles into:

```wat
(module
  (type $t0 (func (result i32)))
  (func $main (export "main") (type $t0) (result i32)
    (i32.sub
      (i32.add
        (i32.const 10)
        (i32.mul
          (i32.div_s
            (i32.const 10)
            (i32.const 5))
          (i32.const 2)))
      (i32.const 1))))
```

### Variable Assignments

`let x = 42; x`

```wat
(module
  (type $t0 (func (result i32)))
  (func $main (export "main") (type $t0) (result i32)
    (local $l0 i32)
    (local.set $l0
      (i32.const 42))
    (local.get $l0)))

```