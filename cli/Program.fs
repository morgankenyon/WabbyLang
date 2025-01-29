namespace Waux.Lang.Cli

module Program =
    open Argu
    open Arguments
    open System

    [<EntryPoint>]
    let main argv =
        let errorHandler =
            ProcessExiter(
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red
            )

        let parser =
            ArgumentParser.Create<CmdArgs>(programName = "waux", errorHandler = errorHandler)

        match parser.ParseCommandLine argv with
        | p when p.Contains(Print) -> runPrint (p.GetResult(Print))
        | c when c.Contains(Compile) ->
            let cc = c.GetResult(Compile)
            compile cc
        | _ ->
            printfn "%s" (parser.PrintUsage())
            Error ArgumentsNotSpecified
        |> getExitCode
