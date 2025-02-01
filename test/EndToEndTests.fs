namespace Waux.Lang.Test

module EndToEndTests =
    open Helpers
    open Xunit
    open Waux.Lang
    open Wasmtime

    [<Theory>]
    [<InlineData("10 + 10 / 5 * 2 - 1", 13)>]
    [<InlineData("5 - 2 + 7", 10)>]
    [<InlineData("10 / 5", 2)>]
    [<InlineData("let x = 42; x", 42)>]
    [<InlineData("let x = 42; let y = 1; x + y", 43)>]
    [<InlineData("let x = 11 - 1; let y = 10 / 5; x + y * 2", 14)>]
    [<InlineData("5 * 7", 35)>]
    [<InlineData("5 - 2 + 7", 10)>]
    [<InlineData("10 - 2", 8)>]
    [<InlineData("5 + 2", 7)>]
    [<InlineData("5", 5)>]
    //[<InlineData(, 2)>]
    let ``Can verify mathematical expression runs correctly`` (expression) (expectedResult) =
        let input = $"func main() {{ {expression} }}"

        let bytes = EndToEnd.compileInstantiateAndPrint input true

        let result = runWithInt32Return bytes

        Assert.Equal(result, expectedResult)

    [<Fact>]
    let ``Can build SymbolMap with nested function definition`` () =
        let input = "func add(x, y) { x + y; } func doIt() { add(1,2); }"

        let symbolScope = EndToEnd.compileToBuildSymbolMap input

        Assert.Equal(1, symbolScope.Count)

        let symbolEntry = symbolScope.First.Value

        match symbolEntry with
        | Wasm.Nested nested -> Assert.Equal(2, nested.Count)
        | Wasm.Locals _ -> raise (new System.Exception("Should be nested"))

    [<Fact>]
    let ``Can test compiling triple nested function`` () =
        let input =
            "func multi(z, v) { z * v; } func add(x, y) { let mul = multi(x, y); mul + x + y; } func main() { add(1,2); }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input true

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runFuncWithInt32Return "main" wasmBytes

        Assert.Equal(5, mainResult)
