# WauxLang

WauxLang - A language that compiles to WASM bytecode.

This is currently an experimental language/compiler targeting WASM as it's runtime. Compiler is written in F#.

Should I use this? Probably not.

This is currently alpha software. You'll run into bugs and issues, potentially for no unexplained reason. Here be dragons.

Why Waux? What does that word mean? How do you pronounce it?
  * Pronounced like the English work "walk" [1](https://www.collinsdictionary.com/us/dictionary/english/wauk)
  * "A Scots word for wake"
  * Why Waux?
    * I googled "words that start with wa" and I liked this one the best.

## Installing as Nuget Tool

The fastest way to get started is to install via nuget tool.

* Install a flavor of .NET 8 from [microsoft](https://dotnet.microsoft.com/en-us/download)
* Run the following command: `dotnet tool install --global Waux.Lang.Cli`
  * To update if already installed: `dotnet tool update -g Waux.Lang.Cli`
* You now have access to the `waux` cli tool

## Running

How to compile and use.

* Write a valid `.waux` program
  * See [examples](./examples/waux/) to get started
  * Every waux file requires a 0 parameter `main` function
  * All code required must be in your `.waux` file, waux does not currently support importing across files.
* Run `waux compile <.waux file>` to generate a wasm file
* Run `waux run <.wasm file>` to run the generated wasm
  * Can also run in any other wasm compliant runtime.

## Language Overview

> Please see the [examples](./examples/waux/) folder for valid programs

Waux currently supports the following language concepts:
* Functions
* While loops
* If/else blocks
* Most boolean comparisons
* Most integer operations
  * Only supports int32 values at the moment
  * Support operator precedence

### Only Supports int32 operations

Right now, waux only supports int32 operations. Any other number types will lead to undefined behavior.

### Simple main function

Lets start with the simpliest program.
```
func main() {
    10
}
```

A main function that returns a value.

### Variables

You can define a variable to hold values using the `let` binding.

```
func main() {
    let val = 23;
    val
}
```

Currently variables names can only be made of alpha characters.

### Mathematical Expressions

You can create more complicated mathematical expressions.

```
func main() {
    let a = 5;
    let b = 10;
    a * 2 + b
}
```

### If/Else Expressions

You can also add if/else expressions in waux:

```
func main() { 
    let p = 10;
    let result = if (p > 20) {
        10
    } else {
        0
    };
    result
}
```

## Code Formatting

This repo currently uses [fantomas](https://fsprojects.github.io/fantomas/) in order to format the compiler.

* To install the tool
  * `dotnet tool install fantomas`
  * `dotnet tool restore`
* To run and format everything
  * `./format.bat`
  * Or `dotnet fantomas -r .`