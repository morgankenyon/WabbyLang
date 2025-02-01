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
        | r when r.Contains(Run) ->
            let rr = r.GetResult(Run)
            run rr
        | c when c.Contains(Compile) ->
            let cc = c.GetResult(Compile)
            compile cc
        | _ ->
            printfn "%s" (parser.PrintUsage())
            Error ArgumentsNotSpecified
        |> getExitCode
