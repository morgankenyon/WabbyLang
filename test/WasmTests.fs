namespace Waux.Lang.Test

module WasmTests =

    open System
    open Xunit
    open Waux.Lang
    open Models
    open Wasmtime
    open Helpers

    [<Fact>]
    let ``Can run simple wasm program`` () =
        let engine = new Engine()

        let modd =
            Module.FromText(
                engine,
                "hello",
                "(module (func $hello (import \"\" \"hello\")) (func (export \"run\") (call $hello)))"
            )

        let linker = new Linker(engine)
        let store = new Store(engine)

        let func: Function =
            Function.FromCallback(store, (fun () -> Console.WriteLine("Hello from C#!")))

        linker.Define("", "hello", func)

        let instance = linker.Instantiate(store, modd)
        let run = instance.GetAction("run")
        run |> ignore
        Assert.True(true)

    [<Fact>]
    let ``Can concat two string`` () =
        let config = (new Config()).WithReferenceTypes(true)
        let engine = new Engine(config)

        let modd =
            Module.FromText(
                engine,
                "concat",
                "(module
              (import \"\" \"concat\" (func $.concat (param externref externref) (result externref)))
              (func (export \"run\") (param externref externref) (result externref)
                local.get 0
                local.get 1
                call $.concat
              )
            )"
            )

        let linker = new Linker(engine)
        let store = new Store(engine)

        // Define the callback using Func with two separate parameters
        linker.Define("", "concat", Function.FromCallback<string, string, string>(store, (fun a b -> $"{a} {b}")))

        let instance = linker.Instantiate(store, modd)
        // Get the function with two separate type parameters
        let run = instance.GetFunction<string, string, string>("run")
        let result = run.Invoke("hello", "world!")

        Assert.Equal("hello world!", result)

    [<Fact>]
    let ``Can run void language`` () =
        let engine = new Engine()

        let bytes =
            [| 0uy
               97uy
               115uy
               109uy
               1uy
               0uy
               0uy
               0uy |]

        let modd = Module.FromBytes(engine, "voidLang", bytes)


        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        Assert.True(true)

    [<Fact>]
    let ``Can run void language from parser methods`` () =
        let engine = new Engine()
        let magic = Wasm.magic ()
        let version = Wasm.version ()

        let bytes = Array.concat [ magic; version ]
        let modd = Module.FromBytes(engine, "voidLang", bytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        Assert.True(true)

    [<Fact>]
    let ``Can handcraft module from bytes`` () =
        let engine = new Engine()
        let magic = Wasm.magic ()
        let version = Wasm.version ()
        let header = Array.concat [ magic; version ]

        let typeSection: byte array =
            [| 1uy //section identifier
               4uy //section size in bytes
               1uy //number of entries that follow
               // type section - entry 0
               96uy //Type `function`
               0uy // Number of parameters
               0uy |] // Number of return values

        let functionSection: byte array =
            [| 3uy //Section identifier
               2uy //section size in bytes
               1uy //number of entries that follow
               // Function section - entry 0
               0uy |] //Index of the type section entry

        let codeSection: byte array =
            [| 10uy //section identifier
               4uy //section size in bytes
               1uy //number of entries that follow
               //code section - entry 0
               2uy //Entry size in bytes
               0uy //Number of local variables
               11uy |] //`end` instruction

        let bytes =
            Array.concat [ header
                           typeSection
                           functionSection
                           codeSection ]
        //printWasm bytes

        let modd = Module.FromBytes(engine, "nopLang", bytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        Assert.True(true)

    [<Fact>]
    let ``Can use helper methods to craft empty module`` () =
        let emptyBytes: byte [] = Array.zeroCreate 0
        let magic = Wasm.magic ()
        let version = Wasm.version ()

        //Creating type section
        let funcType = Wasm.functype (emptyBytes, emptyBytes)
        let typesec = Wasm.typesec ([| funcType |])

        //creating func section
        let funcsec = Wasm.funcsec ([| [| 0uy |] |])

        //creating code section
        let instrEnd = 11uy
        let func = Wasm.func emptyBytes [| instrEnd |]
        let code = Wasm.code func
        let codesec = Wasm.codesec [| code |]

        let bytes =
            Array.concat [ magic
                           version
                           typesec
                           funcsec
                           codesec ]
        //printWasm bytes

        let engine = new Engine()

        let modd = Module.FromBytes(engine, "nopLang", bytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        Assert.True(true)

    [<Fact>]
    let ``Can use helper methods to craft empty module with export`` () =
        let emptyBytes: byte [] = Array.zeroCreate 0
        let magic = Wasm.magic ()
        let version = Wasm.version ()

        //Creating type section
        let funcType = Wasm.functype (emptyBytes, emptyBytes)
        let typesec = Wasm.typesec ([| funcType |])

        //creating func section
        let funcsec = Wasm.funcsec ([| [| 0uy |] |])

        //creating export section
        let exportDesc = Wasm.exportdesc (0uy)
        let export = Wasm.export "main" exportDesc
        let exportsec = Wasm.exportsec [| export |]

        //creating code section
        let instrEnd = 11uy
        let func = Wasm.func emptyBytes [| instrEnd |]
        let code = Wasm.code func
        let codesec = Wasm.codesec [| code |]

        let bytes =
            Wasm.modd [| typesec
                         funcsec
                         exportsec
                         codesec |]
        //printWasm bytes

        let engine = new Engine()

        let modd = Module.FromBytes(engine, "nopLang", bytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        Assert.True(true)

    [<Fact>]
    let ``Can test building symbol table`` () =
        let input = "func add(x, y) { let sum = x + y; sum; }"
        let symbolTable = EndToEnd.compileToBuildSymbolMap input

        Assert.NotNull(symbolTable)
        Assert.Equal(1, symbolTable.Count)

        let symbolEntry = symbolTable.First.Value

        match symbolEntry with
        | Wasm.Nested nested ->
            Assert.Equal(1, nested.Count)
            Assert.True(nested.ContainsKey("add"))

            let (addEntries, addIndex) = nested["add"]

            Assert.Equal(3, addEntries.Count)
            Assert.Equal(0, addIndex)
        | Wasm.Locals locals -> raise (new Exception("Should have been nested"))

    [<Fact>]
    let ``Can test building symbol table of triple nested function`` () =
        let input =
            "func first() { second(1,2); } func second(x, y) { third(x, y); } func third(w,z) { w + z; }"

        let symbolTable = EndToEnd.compileToBuildSymbolMap input

        Assert.NotNull(symbolTable)
        Assert.Equal(1, symbolTable.Count)

        let symbolEntry = symbolTable.First.Value

        match symbolEntry with
        | Wasm.Nested nested ->
            Assert.Equal(3, nested.Count)

            Assert.True(nested.ContainsKey("first"))
            let (firstEntries, firstIndex) = nested["first"]
            Assert.Equal(0, firstEntries.Count)
            Assert.Equal(0, firstIndex)

            Assert.True(nested.ContainsKey("second"))
            let (secondEntries, secondIndex) = nested["second"]
            Assert.Equal(2, secondEntries.Count)
            Assert.Equal(1, secondIndex)

            Assert.True(nested.ContainsKey("third"))
            let (thirdEntries, thirdIndex) = nested["third"]
            Assert.Equal(2, thirdEntries.Count)
            Assert.Equal(2, thirdIndex)

        | Wasm.Locals _ -> raise (new Exception("Should have been nested"))

    [<Fact>]
    let ``Can test compiling simple function`` () =
        let input = "func main() { let x = 42; x }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runFuncWithInt32Return "main" wasmBytes

        Assert.Equal(42, mainResult)

    [<Fact>]
    let ``Can test compiling nested function`` () =
        let input = "func main() { add(1,2); } func add(x, y) { x + y; }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runFuncWithInt32Return "main" wasmBytes

        Assert.Equal(3, mainResult)

    [<Fact>]
    let ``Can test compiling if else statement`` () =
        let input =
            """
func isZero(x) {
    let result = if (x) { 0 } else { 1 };
    result
}"""

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let isZero = runInt32FuncWithInt32Return "isZero" wasmBytes

        Assert.Equal(1, isZero 0)
        Assert.Equal(0, isZero 1)

    [<Theory>]
    [<InlineData("a > b", 43, 42, 1)>]
    [<InlineData("a > b", 42, 43, 0)>]
    [<InlineData("a > b", 42, 42, 0)>]
    [<InlineData("a < b", 42, 43, 1)>]
    [<InlineData("a < b", 43, 42, 0)>]
    [<InlineData("a < b", 42, 42, 0)>]
    [<InlineData("a >= b", 43, 42, 1)>]
    [<InlineData("a >= b", 42, 43, 0)>]
    [<InlineData("a >= b", 42, 42, 1)>]
    [<InlineData("a <= b", 42, 43, 1)>]
    [<InlineData("a <= b", 43, 42, 0)>]
    [<InlineData("a <= b", 42, 42, 1)>]
    [<InlineData("a == b", 43, 42, 0)>]
    [<InlineData("a == b", 42, 43, 0)>]
    [<InlineData("a == b", 42, 42, 1)>]
    [<InlineData("a != b", 43, 42, 1)>]
    [<InlineData("a != b", 42, 43, 1)>]
    [<InlineData("a != b", 42, 42, 0)>]
    [<InlineData("a and b", 0, 0, 0)>]
    [<InlineData("a and b", 1, 0, 0)>]
    [<InlineData("a and b", 0, 1, 0)>]
    [<InlineData("a and b", 1, 1, 1)>]
    [<InlineData("a or b", 0, 0, 0)>]
    [<InlineData("a or b", 1, 0, 1)>]
    [<InlineData("a or b", 0, 1, 1)>]
    [<InlineData("a or b", 1, 1, 1)>]
    let ``Can test binary comparison operators`` operation p1 p2 expectedResult =
        let input = $"func main(a, b) {{ {operation} }}"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let result = runBinaryInt32Expression "main" wasmBytes p1 p2

        Assert.Equal(expectedResult, result)

    [<Fact>]
    let ``Can test assignment functionality`` () =
        let input = "func main(x) { x := x + 13; x }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runInt32FuncWithInt32Return "main" wasmBytes 10

        Assert.Equal(23, mainResult)

    [<Fact>]
    let ``Can test while loop`` () =
        let input = "func countTo(n) { let x = 0; while (x < n) { x := x + 1; } x; }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runInt32FuncWithInt32Return "countTo" wasmBytes 10

        Assert.Equal(10, mainResult)
