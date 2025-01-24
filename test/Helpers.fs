namespace Waux.Lang.Test

module Helpers =
    open Models
    open Wasmtime
    open Waux.Lang
    let runWithInt32Return (wasmBytes : byte array) =
                
        let engine = new Engine()

        let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        let main = instance.GetFunction<int32>("main")
        main.Invoke()

    let printWasm (bytes: byte array) =
        let stringRepresentation = bytes
                                    |> Array.map (fun by -> by.ToString())
                                    |> String.concat ""
        //let str = stringRepresentation |> String.concat ""
        System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
        System.IO.File.WriteAllBytes("./atest.wasm", bytes)

    let buildLetStatement (identifier: string) (value: int) =
        
        let valueTokenPair = { Token = Token.NUMBER; Literal = value.ToString() }
        let value = new Ast.IntegerLiteral(valueTokenPair, value)

        let identifierTokenPair = { Token = Token.IDENT; Literal = identifier }
        let identifier = new Ast.Identifier(identifierTokenPair, identifier)
        
        let letTokenPair = { Token = Token.LET; Literal = "let" }
        let letStatement = new Ast.LetStatement(letTokenPair, identifier, value)

        letStatement

