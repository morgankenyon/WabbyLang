namespace Waux.Lang.Test

module LexerTests =

    open System
    open Xunit
    open Waux.Lang
    open Models

    let AssertTokens (lexer: LexerState, expectedToken) =
        let actualToken = Lexer.nextToken lexer
        Assert.Equal(expectedToken.Token, actualToken.Token)
        Assert.Equal(expectedToken.Literal, actualToken.Literal)
        ()

    let buildTokenTypes (tokens) =
        tokens
        |> List.map (fun (t, l) -> { Token = t; Literal = l })

    [<Theory>]
    [<InlineData("2")>]
    [<InlineData("2242")>]
    [<InlineData("227")>]
    [<InlineData("9283")>]
    let ``Can Lex numbers`` input =
        let expectedTokens =
            [ { Token = Token.NUMBER
                Literal = input }
              { Token = Token.EOF; Literal = "" } ]

        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex Basic Addition`` () =
        let expectedTokens =
            [ { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.PLUS; Literal = "+" }
              { Token = Token.NUMBER; Literal = "1" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "2 + 1"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex Basic Subtraction`` () =
        let expectedTokens =
            [ { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.MINUS; Literal = "-" }
              { Token = Token.NUMBER; Literal = "1" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "2 - 1"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex combined operation Subtraction`` () =
        let expectedTokens =
            [ { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.MINUS; Literal = "-" }
              { Token = Token.NUMBER; Literal = "1" }
              { Token = Token.PLUS; Literal = "+" }
              { Token = Token.NUMBER; Literal = "3" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "2 - 1 + 3"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex multiplication`` () =
        let expectedTokens =
            [ { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.ASTERISK
                Literal = "*" }
              { Token = Token.NUMBER; Literal = "3" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "2 * 3"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex division`` () =
        let expectedTokens =
            [ { Token = Token.NUMBER; Literal = "4" }
              { Token = Token.SLASH; Literal = "/" }
              { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "4 / 2"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex parenthesis`` () =
        let expectedTokens =
            [ { Token = Token.LPAREN; Literal = "(" }
              { Token = Token.NUMBER; Literal = "4" }
              { Token = Token.SLASH; Literal = "/" }
              { Token = Token.NUMBER; Literal = "2" }
              { Token = Token.RPAREN; Literal = ")" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "(4 / 2)"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex let statement`` () =
        let expectedTokens =
            [ { Token = Token.LET; Literal = "let" }
              { Token = Token.IDENT; Literal = "x" }
              { Token = Token.ASSIGN; Literal = "=" }
              { Token = Token.NUMBER; Literal = "3" }
              { Token = Token.SEMICOLON
                Literal = ";" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "let x = 3;"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex let statement with identifier`` () =
        let expectedTokens =
            [ { Token = Token.LET; Literal = "let" }
              { Token = Token.IDENT; Literal = "x" }
              { Token = Token.ASSIGN; Literal = "=" }
              { Token = Token.NUMBER; Literal = "3" }
              { Token = Token.SEMICOLON
                Literal = ";" }
              { Token = Token.IDENT; Literal = "x" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "let x = 3; x"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex assignment`` () =
        let expectedTokens =
            [ { Token = Token.IDENT; Literal = "x" }
              { Token = Token.ASSIGNMENT
                Literal = ":=" }
              { Token = Token.IDENT; Literal = "x" }
              { Token = Token.PLUS; Literal = "+" }
              { Token = Token.NUMBER; Literal = "3" }
              { Token = Token.EOF; Literal = "" } ]

        let input = "x := x + 3"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex simple function declarations`` () =
        let expectedTokensRaw: (Token * string) list =
            [ (Token.FUNC, "func")
              (Token.IDENT, "add")
              (Token.LPAREN, "(")
              (Token.IDENT, "x")
              (Token.COMMA, ",")
              (Token.IDENT, "y")
              (Token.RPAREN, ")")
              (Token.LBRACE, "{")
              (Token.IDENT, "x")
              (Token.PLUS, "+")
              (Token.IDENT, "y")
              (Token.SEMICOLON, ";")
              (Token.RBRACE, "}")
              (Token.SEMICOLON, ";")
              (Token.EOF, "") ]

        let expectedTokens = buildTokenTypes expectedTokensRaw

        let input = "func add(x, y) { x + y; };"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact>]
    let ``Can Lex simple if else statement`` () =
        let expectedTokensRaw: (Token * string) list =
            [ (Token.IF, "if")
              (Token.LPAREN, "(")
              (Token.IDENT, "x")
              (Token.RPAREN, ")")
              (Token.LBRACE, "{")
              (Token.NUMBER, "42")
              (Token.RBRACE, "}")
              (Token.ELSE, "else")
              (Token.LBRACE, "{")
              (Token.NUMBER, "99")
              (Token.RBRACE, "}")
              (Token.EOF, "") ]

        let expectedTokens = buildTokenTypes expectedTokensRaw

        let input = "if (x) { 42 } else { 99 }"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Fact(Skip = "Not implementing elif for now")>]
    let ``Can Lex if else if statement`` () =
        let expectedTokensRaw: (Token * string) list =
            [ (Token.IF, "if")
              (Token.LPAREN, "(")
              (Token.IDENT, "x")
              (Token.RPAREN, ")")
              (Token.LBRACE, "{")
              (Token.NUMBER, "42")
              (Token.RBRACE, "}")
              (Token.ELIF, "elif")
              (Token.LPAREN, "(")
              (Token.IDENT, "x")
              (Token.RPAREN, ")")
              (Token.LBRACE, "{")
              (Token.NUMBER, "32")
              (Token.RBRACE, "}")
              (Token.ELSE, "else")
              (Token.LBRACE, "{")
              (Token.NUMBER, "99")
              (Token.RBRACE, "}")
              (Token.EOF, "") ]

        let expectedTokens = buildTokenTypes expectedTokensRaw

        let input = "if (x) { 42 } elif (x) { 32 } else { 99 }"
        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Theory>]
    [<InlineData("==")>]
    [<InlineData("!=")>]
    [<InlineData("<")>]
    [<InlineData("<=")>]
    [<InlineData(">")>]
    [<InlineData(">=")>]
    [<InlineData("and")>]
    [<InlineData("or")>]
    [<InlineData("while")>]
    let ``Can Lex boolean conditionals`` (input: string) =
        let token = Models.StrToToken input
        let expectedTokensRaw: (Token * string) list = [ (token, input); (Token.EOF, "") ]

        let expectedTokens = buildTokenTypes expectedTokensRaw

        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))

    [<Theory>]
    [<InlineData("func")>]
    [<InlineData("let")>]
    [<InlineData("if")>]
    [<InlineData("else")>]
    [<InlineData("elif")>]
    [<InlineData("while")>]
    let ``Can lex keywords`` (input: string) =
        let token = Models.StrToToken input
        let expectedTokensRaw: (Token * string) list = [ (token, input); (Token.EOF, "") ]

        let expectedTokens = buildTokenTypes expectedTokensRaw

        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))
        
    [<Theory>]
    [<InlineData("x")>]
    [<InlineData("y")>]
    [<InlineData("main")>]
    [<InlineData("addCount")>]
    [<InlineData("add_count")>]
    [<InlineData("p1")>]
    let ``Can lex identifiers`` (input: string) =        
        let expectedTokensRaw: (Token * string) list = [ (Token.IDENT, input); (Token.EOF, "") ]
        let expectedTokens = buildTokenTypes expectedTokensRaw

        let lexer = Lexer.createLexer input

        expectedTokens
        |> List.iter (fun et -> AssertTokens(lexer, et))
        
    [<Theory>]
    [<InlineData("x%")>]
    [<InlineData("yyyy11*")>]
    [<InlineData("add_count:")>]
    let ``Can error out on bad identifiers`` (input: string) =
        let expectedTokensRaw: (Token * string) list = [ (Token.IDENT, input); (Token.EOF, "") ]
        let expectedTokens = buildTokenTypes expectedTokensRaw

        let lexer = Lexer.createLexer input

        let excep =
            Assert.Throws<Exception>(fun () ->                 
                List.iter (fun et -> AssertTokens(lexer, et)) expectedTokens :> obj)

        let lastChar = input.Chars(input.Length - 1)
        Assert.Equal($"'{lastChar}' is not allowed in an identifier", excep.Message)
