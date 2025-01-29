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

Currently I have basic mathematical operations and function calling working. For the examples below, I'm converting the compiled wasm to wat for visual purposes. But the output of the compiler is wasm byte code.

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

### Function Calls

```
func main() { 
  add(1,2); 
}
func add(x, y) {
  let mul = multi(x, y);
  mul + x + y;
} 
func multi(z, v) {
  z * v; 
}
```

```wat
(module
  (type $t0 (func (result i32)))
  (type $t1 (func (param i32 i32) (result i32)))
  (type $t2 (func (param i32 i32) (result i32)))
  (func $main (export "main") (type $t0) (result i32)
    (call $add
      (i32.const 1)
      (i32.const 2)))
  (func $add (export "add") (type $t1) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32)
    (local.set $l2
      (call $multi
        (local.get $p0)
        (local.get $p1)))
    (i32.add
      (i32.add
        (local.get $l2)
        (local.get $p0))
      (local.get $p1)))
  (func $multi (export "multi") (type $t2) (param $p0 i32) (param $p1 i32) (result i32)
    (i32.mul
      (local.get $p0)
      (local.get $p1))))
```

## Code Formatting

This repo currently uses [fantomas](https://fsprojects.github.io/fantomas/) in order to format the compiler.

* To install the tool
  * `dotnet tool install fantomas`
  * `dotnet tool restore`
* To run and format everything
  * `./format.bat`
  * Or `dotnet fantomas -r .`