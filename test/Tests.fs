module Tests

open System
open Xunit
open Waux.Lang
open Wasmtime

let printWasm (bytes: byte array) =
    let stringRepresentation = bytes
                                |> Array.map (fun by -> by.ToString())
                                |> String.concat ""
    //let str = stringRepresentation |> String.concat ""
    System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
    System.IO.File.WriteAllBytes("./atest.wasm", bytes)

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``Can run simple wasm program`` () =
    let engine = new Engine()
    let modd = Module.FromText(engine, "hello", "(module (func $hello (import \"\" \"hello\")) (func (export \"run\") (call $hello)))")

    let linker = new Linker(engine)
    let store = new Store(engine)

    let func: Function = Function.FromCallback(store, fun () -> Console.WriteLine("Hello from C#!"))
    linker.Define(
        "",
        "hello",
        func
    )

    let instance = linker.Instantiate(store, modd)
    let run = instance.GetAction("run")
    run |> ignore
    Assert.True(true)

[<Fact>]
let ``Can concat two string``() =
    let config = (new Config()).WithReferenceTypes(true)
    let engine = new Engine(config)
    let modd = Module.FromText(engine, "concat", "(module
          (import \"\" \"concat\" (func $.concat (param externref externref) (result externref)))
          (func (export \"run\") (param externref externref) (result externref)
            local.get 0
            local.get 1
            call $.concat
          )
        )")
    
    let linker = new Linker(engine)
    let store = new Store(engine)
    
    // Define the callback using Func with two separate parameters
    linker.Define(
        "",
        "concat",
        Function.FromCallback<string, string, string>(store, fun a b -> $"{a} {b}")
    )
    
    let instance = linker.Instantiate(store, modd)
    // Get the function with two separate type parameters
    let run = instance.GetFunction<string, string, string>("run")
    let result = run.Invoke("hello", "world!")
    
    Assert.Equal("hello world!", result)

[<Fact>]
let ``Can run void language``() =
    let engine = new Engine()
    let bytes = [|0uy; 97uy; 115uy; 109uy; 1uy; 0uy; 0uy; 0uy|]
    let modd = Module.FromBytes(engine, "voidLang", bytes)

    
    let linker = new Linker(engine)
    let store = new Store(engine)

    let instance = linker.Instantiate(store, modd)

    Assert.True(true)
[<Fact>]
let ``Can run void language from parser methods``() =
    let engine = new Engine()
    let magic = Wasm.magic()
    let version = Wasm.version()

    let bytes = Array.concat [ magic; version]
    let modd = Module.FromBytes(engine, "voidLang", bytes)

    let linker = new Linker(engine)
    let store = new Store(engine)

    let instance = linker.Instantiate(store, modd)

    Assert.True(true)

[<Fact>]
let ``Can handcraft module from bytes``() =
    let engine = new Engine()
    let magic = Wasm.magic()
    let version = Wasm.version()
    let header = Array.concat [ magic; version ]

    let typeSection: byte array = [|
        1uy; //section identifier
        4uy; //section size in bytes
        1uy; //number of entries that follow
        // type section - entry 0
        96uy; //Type `function`
        0uy; // Number of parameters
        0uy  // Number of return values
        |]
    let functionSection : byte array = [|
        3uy; //Section identifier
        2uy; //section size in bytes
        1uy; //number of entries that follow
        // Function section - entry 0
        0uy //Index of the type section entry
        |]
    let codeSection : byte array = [|
        10uy; //section identifier
        4uy; //section size in bytes
        1uy; //number of entries that follow
        //code section - entry 0
        2uy; //Entry size in bytes
        0uy; //Number of local variables
        11uy //`end` instruction
        |]

    let bytes = Array.concat [ header; typeSection; functionSection; codeSection ]
    //printWasm bytes

    let modd = Module.FromBytes(engine, "nopLang", bytes)

    let linker = new Linker(engine)
    let store = new Store(engine)

    let instance = linker.Instantiate(store, modd)

    Assert.True(true)



[<Fact>]
let ``Can use helper methods to craft empty module``() =
    let emptyBytes: byte [] = Array.zeroCreate 0
    let magic = Wasm.magic()
    let version = Wasm.version()

    //Creating type section
    let funcType = Wasm.functype(emptyBytes, emptyBytes)
    let typesec = Wasm.typesec([| funcType |])

    //creating func section
    let funcsec = Wasm.funcsec([| [|0uy|] |])

    //creating code section
    let instrEnd = 11uy
    let func = Wasm.func emptyBytes [| instrEnd |]
    let code = Wasm.code func
    let codesec = Wasm.codesec [| code |]

    let bytes = Array.concat [ magic; version; typesec; funcsec; codesec ]
    //printWasm bytes

    let engine = new Engine()

    let modd = Module.FromBytes(engine, "nopLang", bytes)

    let linker = new Linker(engine)
    let store = new Store(engine)

    let instance = linker.Instantiate(store, modd)

    Assert.True(true)

[<Fact>]
let ``Can use helper methods to craft empty module with export``() =
    let emptyBytes: byte [] = Array.zeroCreate 0
    let magic = Wasm.magic()
    let version = Wasm.version()

    //Creating type section
    let funcType = Wasm.functype(emptyBytes, emptyBytes)
    let typesec = Wasm.typesec([| funcType |])

    //creating func section
    let funcsec = Wasm.funcsec([| [|0uy|] |])

    //creating export section
    let exportDesc = Wasm.exportdesc(0uy)
    let export = Wasm.export "main" exportDesc
    let exportsec = Wasm.exportsec [| export |]

    //creating code section
    let instrEnd = 11uy
    let func = Wasm.func emptyBytes [| instrEnd |]
    let code = Wasm.code func
    let codesec = Wasm.codesec [| code |]

    let bytes = Wasm.modd [| typesec; funcsec; exportsec; codesec |]
    printWasm bytes

    let engine = new Engine()

    let modd = Module.FromBytes(engine, "nopLang", bytes)

    let linker = new Linker(engine)
    let store = new Store(engine)

    let instance = linker.Instantiate(store, modd)

    Assert.True(true)