namespace Waux.Lang.Test

module LexerTests = 

    open System
    open Xunit
    open Models
    let AssertTokens(lexer: LexerState, expectedToken) =
        let actualToken = Lexer.nextToken lexer
        Assert.Equal(expectedToken.Token, actualToken.Token)
        Assert.Equal(expectedToken.Literal, actualToken.Literal)
        ()

    let buildTokenTypes(tokens) =
        tokens |> List.map (fun (t, l) -> { Token = t ; Literal = l })

    [<Theory>]
    [<InlineData("2")>]
    [<InlineData("2242")>]
    [<InlineData("227")>]
    [<InlineData("9283")>]
    let ``Can Lex numbers`` input =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = input }
                { Token = Token.EOF ; Literal = "" };
            ]

        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex Basic Arithmetic`` () =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.PLUS ; Literal = "+" }
                { Token = Token.NUMBER ; Literal = "1" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "2 + 1"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))