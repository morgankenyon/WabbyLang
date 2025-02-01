﻿namespace Waux.Lang.Cli

module Arguments =
    open Argu
    open Wasmtime
    open Waux.Lang

    type CliError = | ArgumentsNotSpecified
    type Filename = string

    type CompileArgs =
        | [<MainCommand>] Filename of file: string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Filename _ -> "The .waux file to compile"

    type RunArgs =
        | [<MainCommand>] Filename of file: string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Filename _ -> "The .wasm file to run"

    type CmdArgs =
        | [<CliPrefix(CliPrefix.None)>] Compile of ParseResults<CompileArgs>
        | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<RunArgs>
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Run _ -> "Runs a .wasm file"
                | Compile _ -> "Compile a .waux file to .wasm"

    let getExitCode result =
        match result with
        | Ok () -> 0
        | Error err ->
            match err with
            | ArgumentsNotSpecified -> 1

    let runPrint print =
        printfn "%s" print
        Ok()

    let compile (parseResults: ParseResults<CompileArgs>) =
        match parseResults with
        | f when f.Contains(CompileArgs.Filename) ->
            let filename = f.GetResult(CompileArgs.Filename)
            printfn "Compiling: %s" filename

            let fileText = System.IO.File.ReadAllText filename

            //printfn "%s" fileText
            let name = System.IO.Path.GetFileNameWithoutExtension filename
            let fileInfo = new System.IO.FileInfo(filename)
            let directory = fileInfo.DirectoryName
            let wauxFilename = $"{directory}\{name}.wasm"

            let wasmBytes = EndToEnd.compileModule fileText

            System.IO.File.WriteAllBytes(wauxFilename, wasmBytes)
            printfn "Compiled to: %s" wauxFilename
            Ok()
        | _ -> Error ArgumentsNotSpecified

    let run (parseResults: ParseResults<RunArgs>) =
        match parseResults with
        | f when f.Contains(RunArgs.Filename) ->
            let filename = f.GetResult(RunArgs.Filename)
            printfn "Running: %s" filename
            let funcName = "main"

            let wasmBytes = System.IO.File.ReadAllBytes filename

            let engine = new Engine()

            let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

            let linker = new Linker(engine)
            let store = new Store(engine)

            let instance = linker.Instantiate(store, modd)

            let main = instance.GetFunction<int32>(funcName)
            let result = main.Invoke()
            printfn "%d" result

            Ok()
        | _ -> Error ArgumentsNotSpecified
