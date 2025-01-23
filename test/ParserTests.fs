namespace Waux.Lang.Test

module ParserTests =
    open Xunit
    open Waux.Lang
    let isInteger (e : Ast.Expression) =
        match e with
        | :? Ast.IntegerLiteral as id -> true
        | _ -> false
    let AssertNoParseErrors (p: Parser.ParserState) =
        //if errors, maybe print to err out
        //TODO - maybe include parser errors in the output
        //TODO - maybe print to stderr
        let errorMessage =  
            if p.errors.Count > 0 then 
                let error = p.errors.ToArray() |> Array.reduce (fun a b -> sprintf "%s\n%s" a b)
                error
            else ""
        Assert.True(0 = p.errors.Count, errorMessage)
    [<Fact>]
    let ``Can test integer expression`` () =
        let input = "5"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(isInteger(modd.expressions.[0]))

        let literal = modd.expressions.[0] :?> Ast.IntegerLiteral

        Assert.Equal(5, literal.value)// 5L

