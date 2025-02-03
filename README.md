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
  * See [examples](./examples/) to get started
  * Every waux file requires a 0 parameter `main` function
  * All code must be in a single `.waux` file, waux does not currently support importing across files.
* Run `waux compile <.waux file>` to generate a wasm file
* Run `waux run <.wasm file>` to run the generated wasm
  * Can also run in any other wasm compliant runtime.
  * This leverages the [wasmtime](https://www.nuget.org/packages/Wasmtime) nuget package.

## Language Overview

> Please see the [examples](./examples/) folder for valid programs

Waux currently supports the following language concepts:
* Functions
* While loops
* If/else blocks
* Most boolean comparisons
* Most integer operations
  * With clarification below
  * Support operator precedence

### int32 support clarification

WebAssembly stores numbers in memory in variable width. Right now Waux works with int32s that can be stored in one byte.

This will be fixed in an upcoming release.

### Simple main function

Lets start with the simpliest program.
```
func main() {
    10
}
```

A main function that returns a hard coded value.

### Variables

You can declare and define a variable to hold values using the `let` binding.

```
func main() {
    let val = 23;
    val
}
```

Once a variable has been declared, you change change the value by using the `:=` operator.

```
func main() {
    let val = 23;
    val := val - 11;
    val
}
```

Status:
* Currently variables and function names can be made of the following values:
  * Alpha characters: [a-zA-Z]
  * Digit characters: [0-9]
  * Underscore: [_]
  * CombinedRegex: [a-zA-Z_0-9]+
* There is currently no way to define an empty/null variable `let x;`


### Semicolons

Semicolons separate expressions, the last semicolon is unneeded to indicate a return type. Future work may drop the need for semicolons.

General rule of thumb:
* Add semicolon to all normal lines of code
* Blocks of code `{}` do not need a semicolon after the closing block `}`.
* Return values do not need a semicolon
* But last line in a block that does not return a value (currently only `while` loop), need a semicolon.
  * See the euler1 example below.
  * `if/else` is currently an expression, so last line is assumed the return value. Again, see euler1 example below.

> There is still some undefined behavior around semicolons. When I'm debugging sometimes I'll add semicolons to see if that fixes anything. Alpha code, am I right???

### Mathematical Expressions

You can create more complicated mathematical expressions.

```
func main() {
    let a = 5;
    let b = 10;
    a * 2 + b
}
```

and another

```
func main() {
    let a = 5;
    let b = 10;
    a * (2 + b)
}
```

Status:
* All four primary mathematical operations (+, -, *, /)
  * Plus modulo (%)
* Operator precedence (* before +, etc)
* Use of parentheses to denotate precedence

### Boolean Logic

Currently Waux does not contain a true boolean value `true/false`. WebAssembly also does not contain a true boolean value.

For comparison, it currently uses 0 meaning false and 1 meaning true.

So it supports all the following boolean operators but just on integer values instead of `true/false`
* `==` - equal
* `!=` - not equal
* `<` - less than
* `<=` - less than equal
* `>` - greater than
* `>=` - greater than equal
* `and` - only true if both expressions are true
* `or` - true if either expressions are true

All these operators are binary expressions in the form of: `<expression1> <operator> <expression2>`.

You can utilize boolean logic with both of the following concepts.

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

Status:
* Since `if/else` is an expression (returns a value), no semicolon is needed on the last lines of each branch.
  * In the example above, `result` is set as the value of the `if/else` expressions.
* Currently `else if` statement is not supported
* Right now the `else` is optional, but will be changing in a future release

### While Loops

You can leverage a while loop like below.
```
func countTo(n) {
    let x = 0; 
    while (x < n) {
        x := x + 1; 
    }
    x
}

func main() {
    countTo(10)
}
```

Status:
* `break` or `continue` keywords are currently supported
* Since `while` loop is a statement instead of an expression, a semicolon is needed on the last line in the while loop.

### Euler1

Putting this all together, we have a solution for [euler1](https://projecteuler.net/problem=1):
```
func main() {
    let count = 0;
    let n = 1;
    while (n < 1000) {
        let num = if (((n % 3 == 0) or ((n % 5) == 0))) {
            n
        } else {
            0
        };
        count := count + num;
        n := n + 1;
    }
    count
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