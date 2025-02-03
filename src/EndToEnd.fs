namespace Waux.Lang

module EndToEnd =

    let private printWasm (bytes: byte array) =
        let stringRepresentation =
            bytes
            |> Array.map (fun by -> by.ToString())
            |> String.concat "; "
        //let str = stringRepresentation |> String.concat ""
        System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
        System.IO.File.WriteAllBytes("./atest.wasm", bytes)
        bytes

    let compileToBuildSymbolMap (input: string) =
        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser
        let scopes = Wasm.buildSymbolMap modd
        scopes

    let internal compileModuleDebug (input: string) =
        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser
        let wasmBytes = Wasm.compile modd

        wasmBytes

    let compileModule (input: string) =
        let wasmBytes =
            Lexer.createLexer input
            |> Parser.createParser
            |> Parser.parseModule
            |> Wasm.compile

        wasmBytes

    let compileModuleAndPrint (input: string) (print: bool) =
        let wasmBytes =
            if print then
                compileModule input |> printWasm
            else
                compileModule input

        wasmBytes
