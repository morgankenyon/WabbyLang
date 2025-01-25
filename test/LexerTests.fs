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
    let ``Can Lex Basic Addition`` () =
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

    [<Fact>]
    let ``Can Lex Basic Subtraction`` () =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.MINUS ; Literal = "-" }
                { Token = Token.NUMBER ; Literal = "1" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "2 - 1"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex combined operation Subtraction`` () =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.MINUS ; Literal = "-" }
                { Token = Token.NUMBER ; Literal = "1" }
                { Token = Token.PLUS ; Literal = "+" }
                { Token = Token.NUMBER ; Literal = "3" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "2 - 1 + 3"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex multiplication`` () =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.ASTERISK ; Literal = "*" }
                { Token = Token.NUMBER ; Literal = "3" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "2 * 3"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex division`` () =
        let expectedTokens =
            [
                { Token = Token.NUMBER ; Literal = "4" }
                { Token = Token.SLASH ; Literal = "/" }
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "4 / 2"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex parenthesis`` () =
        let expectedTokens =
            [
                { Token = Token.LPAREN ; Literal = "(" }
                { Token = Token.NUMBER ; Literal = "4" }
                { Token = Token.SLASH ; Literal = "/" }
                { Token = Token.NUMBER ; Literal = "2" }
                { Token = Token.RPAREN ; Literal = ")" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "(4 / 2)"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex let statement`` () =
        let expectedTokens =
            [
                { Token = Token.LET ; Literal = "let" }
                { Token = Token.IDENT ; Literal = "x" }
                { Token = Token.ASSIGN ; Literal = "=" }
                { Token = Token.NUMBER ; Literal = "3" }
                { Token = Token.SEMICOLON ; Literal = ";" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "let x = 3;"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex let statement with identifier`` () =
        let expectedTokens =
            [
                { Token = Token.LET ; Literal = "let" }
                { Token = Token.IDENT ; Literal = "x" }
                { Token = Token.ASSIGN ; Literal = "=" }
                { Token = Token.NUMBER ; Literal = "3" }
                { Token = Token.SEMICOLON ; Literal = ";" }
                { Token = Token.IDENT ; Literal = "x" }
                { Token = Token.EOF ; Literal = "" };
            ]
    
        let input = "let x = 3; x"
        let lexer = Lexer.createLexer input
        expectedTokens |> List.iter (fun et -> AssertTokens(lexer, et))