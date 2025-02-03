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
        let exportDesc = Wasm.exportdesc [|0uy|]
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
            "func third(w,z) { w + z; } func second(x, y) { third(x, y); } func first() { second(1,2); }"

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
            Assert.Equal(2, firstIndex)

            Assert.True(nested.ContainsKey("second"))
            let (secondEntries, secondIndex) = nested["second"]
            Assert.Equal(2, secondEntries.Count)
            Assert.Equal(1, secondIndex)

            Assert.True(nested.ContainsKey("third"))
            let (thirdEntries, thirdIndex) = nested["third"]
            Assert.Equal(2, thirdEntries.Count)
            Assert.Equal(0, thirdIndex)

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
        let input = "func add(x, y) { x + y; } func main() { add(1,2); }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input true

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runFuncWithInt32Return "main" wasmBytes

        Assert.Equal(3, mainResult)

    [<Fact>]
    let ``Can test compiling if else statement`` () =
        let zeroInput =
            """
func main() {
    let x = 0;
    let result = if (x) { 0 } else { 1 };
    result
}"""
        let oneInput =
            """
func main() {
    let x = 1;
    let result = if (x) { 0 } else { 1 };
    result
}"""

        let zeroBytes = EndToEnd.compileModule zeroInput
        let isZero = runFuncWithInt32Return "main" zeroBytes
        let oneBytes = EndToEnd.compileModule oneInput
        let isOne = runFuncWithInt32Return "main" zeroBytes

        Assert.Equal(1, isZero)
        Assert.Equal(1, isOne)

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
        let input = $"func calc(a, b) {{ {operation} }} func main() {{ calc({p1}, {p2}); }}"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let result = runWithInt32Return wasmBytes

        Assert.Equal(expectedResult, result)

    [<Theory>]
    [<InlineData("a + b", 10, 10, 20)>]
    [<InlineData("a - b", 10, 5, 5)>]
    [<InlineData("a * b", 5, 3, 15)>]
    [<InlineData("a / b", 10, 2, 5)>]
    [<InlineData("a / b", 10, 3, 3)>]
    [<InlineData("a % b", 10, 3, 1)>]
    let ``Can test binary arithmetic operators`` operation p1 p2 expectedResult =
        let input = $"func calc(a, b) {{ {operation} }} func main() {{ calc({p1}, {p2}); }}"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let result = runWithInt32Return wasmBytes

        Assert.Equal(expectedResult, result)

    [<Fact>]
    let ``Can test assignment functionality`` () =
        let input = "func main() { let x = 10; x := x + 13; x }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runFuncWithInt32Return "main" wasmBytes

        Assert.Equal(23, mainResult)

    [<Fact>]
    let ``Can test while loop`` () =
        let input = "func countTo(n) { let x = 0; while (x < n) { x := x + 1; } x; } func main() { countTo(10) }"

        let wasmBytes = EndToEnd.compileModuleAndPrint input false

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runWithInt32Return wasmBytes

        Assert.Equal(10, mainResult)

    [<Fact>]
    let ``Can throw error when encountering an undefined function`` () =
        let input = "func main() { add(1,2); } func add(x, y) { x + y; }"

        let excep =
            Assert.Throws<Exception>(fun () -> EndToEnd.compileModuleAndPrint input false :> obj)

        Assert.Equal("The 'add' function has not been defined before being called", excep.Message)

    [<Fact>]
    let ``Can throw error when program does not have a main function`` () =
        let input = "func test() { 1 + 3; }"

        let excep =
            Assert.Throws<Exception>(fun () -> EndToEnd.compileModuleAndPrint input false :> obj)

        Assert.Equal("Waux requires a zero parameter 'main' function to exist", excep.Message)

    [<Fact>]
    let ``Can throw error when program has arguments in main function`` () =
        let input = "func main(x) { x; }"

        let excep =
            Assert.Throws<Exception>(fun () -> EndToEnd.compileModuleAndPrint input false :> obj)

        Assert.Equal("Waux requires a zero parameter 'main' function to exist", excep.Message)

    [<Fact>]
    let ``Can ensure if/else statment works as expected``() =
        let input = """
func main() { 
  let p = 10;
  let result = if (p > 20) {
    1
  } else {
    0
  };
  result
}"""
        let wasmBytes = EndToEnd.compileModuleDebug input

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runWithInt32Return wasmBytes

        Assert.Equal(0, mainResult)

    [<Fact>]
    let ``Can ensure variable set in while loop works as expected``() =
        let input = """
func main() {
    let n = 1;
    while (n < 10) {
        let count = 1;
        n := n + count;
    };
    n
}"""
        let wasmBytes = EndToEnd.compileModuleDebug input

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runWithInt32Return wasmBytes

        Assert.Equal(10, mainResult)

    [<Fact>]
    let ``Can handle more complicated if statement``() =
        let input = """
func main() {
    let n = 5;
    let num = if ((n % 3 == 0) or (n % 5 == 0)) {
        n
    } else {
        0
    };
    num
}"""
        let wasmBytes = EndToEnd.compileModuleDebug input

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runWithInt32Return wasmBytes

        Assert.Equal(5, mainResult)

    [<Fact>]
    let ``Can handle complicated or inside while loop``() =
        let input = """
func main() {
    let count = 0;
    let n = 1;
    while (n < 10) {
        let num = if (((n % 3 == 0) or ((n % 5) == 0))) {
            n;
        } else {
            0;
        };
        count := count + num;
        n := n + 1;
    };
    count
}"""
        let wasmBytes = EndToEnd.compileModuleAndPrint input true

        Assert.True(wasmBytes.Length > 0)

        let mainResult = runWithInt32Return wasmBytes

        Assert.Equal(23, mainResult)

    let getUnsignedExpectedBytes (num: uint32) =
        match num with
        | 2u -> [| 2uy |]
        | 64u -> [| 64uy |]
        | 127u -> [| 127uy |]
        | 128u -> [| 128uy; 1uy |]
        | 16383u -> [| 255uy; 127uy |]
        | 16384u -> [| 128uy; 128uy; 1uy |]
        | 283828u -> [| 180uy; 169uy; 17uy |]
        | 4_294_967_295u -> [| 255uy; 255uy; 255uy; 255uy; 15uy|]
        | _ -> [||]

    [<Theory>]
    [<InlineData(2u, 1)>]
    [<InlineData(64u, 1)>]
    [<InlineData(127u, 1)>]
    [<InlineData(128u, 2)>]
    [<InlineData(16383u, 2)>]
    [<InlineData(16384u, 3)>]
    [<InlineData(283828u, 3)>]
    [<InlineData(4_294_967_295u, 5)>]
    let ``Can encode unsigned integer via LEB128`` num expectedLength =
        let lebEncoded = Wasm.u32 num

        Assert.Equal(expectedLength, lebEncoded.Length)

        let expectedBytes = getUnsignedExpectedBytes num

        if expectedLength = 5 then
            Assert.Equal(expectedBytes[4], lebEncoded[4])
            Assert.Equal(expectedBytes[3], lebEncoded[3])
            Assert.Equal(expectedBytes[2], lebEncoded[2])
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
        elif expectedLength = 3 then
            Assert.Equal(expectedBytes[2], lebEncoded[2])
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
            
        elif expectedLength = 2 then
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
        else
            Assert.Equal(expectedBytes[0], lebEncoded[0])

    let i32 (v: int32) : byte array =
        let SEVEN_BIT_MASK = 127
        let CONTINUATION_BIT : byte = 128uy
        let mutable vall = v
        let mutable r : byte array = [||]
        let mutable more = true
        let signBit = 64uy
        while more do
            let b : byte = (byte)(vall &&& SEVEN_BIT_MASK)
            let signBitSet = (b &&& signBit) <> 0uy
            
            vall <- vall >>> 7

            let nextVall = 
                if ((vall = 0 && (not signBitSet)) || (vall = -1 && signBitSet)) then
                    more <- false                
                    b
                else
                    b ||| CONTINUATION_BIT
            r <- Array.concat [ r; [| nextVall |] ]
        r
        
    let getSignedExpectedBytes (num: int32) =
        match num with
        | 1 -> [| 1uy |]
        | -1 -> [| 127uy |]
        | 63 -> [| 63uy |]
        | 64 -> [| 192uy; 0uy |]
        | -64 -> [| 64uy |]
        | -65 -> [| 191uy; 127uy |]
        | 127 -> [| 255uy; 0uy |]
        | 128 -> [| 128uy; 1uy |]
        | -128 -> [| 128uy; 127uy |]
        | -129 -> [| 255uy; 126uy |]
        | 7196 -> [| 156uy; 56uy |]
        | 8192 -> [| 128uy; 192uy; 0uy |]
        | -7196 -> [| 228uy; 71uy |]
        | -8193 -> [| 255uy; 191uy; 127uy |]
        | 283828 -> System.Convert.FromHexString("B4A911")
        | 2_147_483_647 -> System.Convert.FromHexString("FFFFFFFF07")
        | -283828 -> System.Convert.FromHexString("CCD66E")
        | -2_147_483_648 -> System.Convert.FromHexString("8080808078")
        | _ -> [||]

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(-1)>]
    [<InlineData(63)>]
    [<InlineData(64)>]
    [<InlineData(-64)>]
    [<InlineData(-65)>]
    [<InlineData(127)>]
    [<InlineData(128)>]
    [<InlineData(-128)>]
    [<InlineData(-129)>]
    [<InlineData(7196)>]
    [<InlineData(8192)>]
    [<InlineData(-7196)>]
    [<InlineData(-8193)>]
    [<InlineData(283828)>]
    [<InlineData(2_147_483_647)>]
    [<InlineData(-283828)>]
    [<InlineData(-2_147_483_648)>]
    let ``Can encode signed integer via LEB128`` num =
        let lebEncoded = i32 num
        let expectedBytes = getSignedExpectedBytes num

        Assert.Equal(expectedBytes.Length, lebEncoded.Length)

        let expectedLength = expectedBytes.Length

        if expectedLength = 5 then
            Assert.Equal(expectedBytes[4], lebEncoded[4])
            Assert.Equal(expectedBytes[3], lebEncoded[3])
            Assert.Equal(expectedBytes[2], lebEncoded[2])
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
        elif expectedLength = 3 then
            Assert.Equal(expectedBytes[2], lebEncoded[2])
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
            
        elif expectedLength = 2 then
            Assert.Equal(expectedBytes[1], lebEncoded[1])
            Assert.Equal(expectedBytes[0], lebEncoded[0])
        else
            Assert.Equal(expectedBytes[0], lebEncoded[0])