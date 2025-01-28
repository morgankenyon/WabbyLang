namespace Waux.Lang.Cli

module Arguments =
    open Argu
    open Waux.Lang

    type CliError =
    | ArgumentsNotSpecified

    [<CliPrefix(CliPrefix.Dash)>]
    type CompileArgs =
        | Filename of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Filename _ -> "The .waux file needing to compile"

    type CmdArgs =
        | [<AltCommandLine("-p")>] Print of message:string
        | [<CliPrefix(CliPrefix.None)>] Compile of ParseResults<CompileArgs>
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Print _ -> "Print a message"
                | Compile _ -> "Compile a .waux file to .wasm"

    let getExitCode result =
        match result with
        | Ok () -> 0
        | Error err ->
            match err with
            | ArgumentsNotSpecified -> 1

    let runPrint print = 
        printfn "%s" print
        Ok ()

    let compile (parseResults: ParseResults<CompileArgs>) =
        match parseResults with
        | f when f.Contains(Filename) ->
            let filename = f.GetResult(Filename)
            printfn "Compiling: %s" filename

            let fileText = System.IO.File.ReadAllText filename

            let name = System.IO.Path.GetFileNameWithoutExtension filename
            let fileInfo = new System.IO.FileInfo(filename)
            let directory = fileInfo.DirectoryName
            //printfn "%s" directory
            let wauxFilename = $"{directory}\{name}.wasm"

            //printfn "%s" wauxFilename
            let wasmBytes = EndToEnd.compileModule fileText

            System.IO.File.WriteAllBytes(wauxFilename, wasmBytes)
            Ok ()
        | _ -> Error ArgumentsNotSpecified

