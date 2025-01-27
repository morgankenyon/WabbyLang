namespace Waux.Lang.Test

module EndToEndTests =
    open Xunit
    open Waux.Lang
    open Wasmtime

    [<Fact>]
    let ``Can compile empty number`` () =
        let input = "5"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile addition operation`` () =
        let input = "5 + 2"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile subtraction operation`` () =
        let input = "10 - 2"

        let result = 
            EndToEnd.compileInstantiateAndPrint input false
            |> Helpers.runWithInt32Return

        Assert.Equal(8, result)

    [<Fact>]
    let ``Can compile combined operation`` () =
        let input = "5 - 2 + 7"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile multiplication operation`` () =
        let input = "5 * 7"

        let result = 
            EndToEnd.compileInstantiateAndPrint input false
            |> Helpers.runWithInt32Return

        Assert.Equal(result, 35)

    [<Fact>]
    let ``Can compile division operation`` () =
        let input = "10 / 5"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 2)

    [<Fact>]
    let ``Can compile and run combined operation`` () =
        let input = "5 - 2 + 7"

        let bytes = EndToEnd.compile input

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 10)

    [<Fact>]
    let ``Can compile all operation formula`` () =
        let input = "10 + 10 / 5 * 2 - 1"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 13)

    [<Fact>]
    let ``Can compile and run simple let statement`` () =
        let input = "let x = 42; x"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 42)

    [<Fact>]
    let ``Can compile and run double let statement`` () =
        let input = "let x = 42; let y = 1; x + y"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 43)

    [<Fact>]
    let ``Can compile and run more complex let statements`` () =
        let input = "let x = 11 - 1; let y = 10 / 5; x + y * 2"

        let bytes = EndToEnd.compileInstantiateAndPrint input true

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 14)

    [<Fact>]
    let ``Can compile with toWasmFlat with simple let statement`` () =
        let input = "let x = 10; x"

        let bytes = EndToEnd.compileToWasmFlat input

        let expectedBytes = 
            [| 
                Wasm.INSTR_i32_CONST; 
                Wasm.i32 10;
                Wasm.INSTR_LOCAL_SET;
                Wasm.i32 0;
                Wasm.INSTR_LOCAL_GET;
                Wasm.i32 0;
                Wasm.INSTR_END
            |]

        Assert.Equivalent(expectedBytes, bytes)

    [<Fact>]
    let ``Can compile with toWasmFlat parameterless function definition`` () =
        let input = "func add() { 42; }"

        let bytes = EndToEnd.compileToWasmFlatDebug input

        let expectedBytes = 
            [| 
                Wasm.INSTR_i32_CONST; 
                Wasm.i32 42;
                Wasm.INSTR_END
            |]

        Assert.Equal(expectedBytes.Length, bytes.Length)
        Assert.Equivalent(expectedBytes, bytes)

    [<Fact>]
    let ``Can compile with toWasmFlat simple function definition`` () =
        let input = "func add() { let x = 0; x }"

        let bytes = EndToEnd.compileToWasmFlatDebug input

        let expectedBytes = 
            [| 
                Wasm.INSTR_i32_CONST;
                Wasm.i32 0;
                Wasm.INSTR_LOCAL_SET;
                Wasm.i32 0;
                Wasm.INSTR_LOCAL_GET;
                Wasm.i32 0;
                Wasm.INSTR_END
            |]

        Assert.Equal(expectedBytes.Length, bytes.Length)
        Assert.Equivalent(expectedBytes, bytes)

    [<Fact>]
    let ``Can build SymbolMap with nested function definition`` () =
        let input = "func doIt() { add(1,2); } func add(x, y) { x + y; }"

        let symbolScope = EndToEnd.compileToBuildSymbolMap2 input
        
        Assert.Equal(1, symbolScope.Count)

        let symbolEntry = symbolScope.First.Value

        match symbolEntry with
        | Wasm.Nested nested ->
            Assert.Equal(2, nested.Count)
        | Wasm.Locals _ ->
            raise (new System.Exception("Should be nested"))