module Tests

open System
open Xunit
open Wasmtime

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