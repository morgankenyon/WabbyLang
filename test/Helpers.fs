﻿namespace Waux.Lang.Test

module Helpers =
    open Models
    open Wasmtime
    open Waux.Lang

    let private buildInstance (wasmBytes: byte array) =
        let engine = new Engine()

        let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        linker.Instantiate(store, modd)

    let runFuncWithInt32Return (funcName: string) (wasmBytes: byte array) =
        let instance = buildInstance wasmBytes

        let main = instance.GetFunction<int32>(funcName)
        main.Invoke()

    let runInt32FuncWithInt32Return (funcName: string) (wasmBytes: byte array) (param: int) =
        let instance = buildInstance wasmBytes

        let func = instance.GetFunction<int32, int32>(funcName)
        func.Invoke(param)

    let runBinaryInt32Expression (funcName: string) (wasmBytes: byte array) (p1: int) (p2: int) =
        let instance = buildInstance wasmBytes

        let func = instance.GetFunction<int32, int32, int32>(funcName)
        func.Invoke(p1, p2)


    let runWithInt32Return (wasmBytes: byte array) = runFuncWithInt32Return "main" wasmBytes

    let printWasm (bytes: byte array) =
        let stringRepresentation =
            bytes
            |> Array.map (fun by -> by.ToString())
            |> String.concat "; "
        //let str = stringRepresentation |> String.concat ""
        System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
        System.IO.File.WriteAllBytes("./atest.wasm", bytes)

    let buildLetStatement (identifier: string) (value: int) =

        let valueTokenPair =
            { Token = Token.NUMBER
              Literal = value.ToString() }

        let value = new Ast.IntegerLiteral(valueTokenPair, value)

        let identifierTokenPair =
            { Token = Token.IDENT
              Literal = identifier }

        let identifier = new Ast.Identifier(identifierTokenPair, identifier)

        let letTokenPair = { Token = Token.LET; Literal = "let" }
        let letStatement = new Ast.LetStatement(letTokenPair, identifier, value)

        letStatement

    let buildIdentifierStatement (identifier: string) =
        let identifierTokenPair =
            { Token = Token.IDENT
              Literal = identifier }

        let identifier = new Ast.Identifier(identifierTokenPair, identifier)

        let expressionStatement =
            new Ast.ExpressionStatement(identifierTokenPair, identifier)

        expressionStatement

    let compileInstantiateAndPrint (input: string) (print: bool) =
        let wasmBytes = EndToEnd.compileModuleAndPrint input print

        let engine = new Engine()

        let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        linker.Instantiate(store, modd) |> ignore

        wasmBytes
