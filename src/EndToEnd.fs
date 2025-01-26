namespace Waux.Lang

module EndToEnd =
    open Wasmtime
    let private printWasm (bytes: byte array) =
        let stringRepresentation = bytes
                                    |> Array.map (fun by -> by.ToString())
                                    |> String.concat ""
        //let str = stringRepresentation |> String.concat ""
        System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
        System.IO.File.WriteAllBytes("./atest.wasm", bytes)
        bytes

    let compileToWasmFlat (input : string) =
        let wasmBytes =
            Lexer.createLexer input
            |> Parser.createParser
            |> Parser.parseModule
            |> Wasm.toWasmFlat
        wasmBytes

    let compileToWasmFlatDebug (input : string) =
        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser
        let wasmBytes = Wasm.toWasmFlat modd
        wasmBytes

    let compile (input: string) =
        let wasmBytes = 
            Lexer.createLexer input
            |> Parser.createParser
            |> Parser.parseModule
            |> Wasm.toWasm

        wasmBytes
    
    let compileInstantiateAndPrint (input: string) (print: bool) =
        let wasmBytes =
            if print then
                compile input
                |> printWasm
            else
                compile input
        let engine = new Engine()

        let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        linker.Instantiate(store, modd) |> ignore

        wasmBytes


