namespace Waux.Lang

module Parser =
    open Models

    type prefixParse = ParserState -> Ast.Expression option
    and infixParse = ParserState -> Ast.Expression -> Ast.Expression option

    and ParserState = 
        {
            lexer : Models.LexerState
            mutable curToken : TokenPair
            mutable peekToken : TokenPair
            mutable errors : ResizeArray<string> //TODO - maybe change to option type??
            prefixParseFns : System.Collections.Generic.Dictionary<Token, prefixParse>
            infixParseFns : System.Collections.Generic.Dictionary<Token, infixParse>
        }

    let toExpr exp = exp :> Ast.Expression

    let toSome (exp: Ast.Expression) = Some exp

    let toSomeExpr exp =
        exp
        |> toExpr
        |> toSome

    let curTokenIs (p: ParserState) (t: Token) =
        p.curToken.Token = t

    let nextToken (p: ParserState) =
        p.curToken <- p.peekToken
        p.peekToken <- Lexer.nextToken p.lexer

    let parseIntegerLiteral p =
        match System.Int32.TryParse p.curToken.Literal with
        | true, l ->
            new Ast.IntegerLiteral(p.curToken, l)
            |> toSomeExpr
        | _ ->
            let errorMsg = sprintf "could not parse %s as integer" p.curToken.Literal
            p.errors.Add(errorMsg)
            None

    let parseExpression p = // precedence =
        match p.prefixParseFns.ContainsKey p.curToken.Token with
        | true -> 
            let prefix = p.prefixParseFns.[p.curToken.Token]
            let mutable leftExp = prefix p

            //while not (peekTokenIs p TokenType.SEMICOLON) && precedence < peekPrecedence p do
                
            //    leftExp <- match p.infixParseFns.ContainsKey p.peekToken.TokenType && leftExp.IsSome with
            //                | true ->
            //                    let infixFn = p.infixParseFns.[p.peekToken.TokenType]
            //                    nextToken p
            //                    infixFn p leftExp.Value
            //                | _ -> leftExp
            leftExp
        | false -> 
            p.errors.Add(sprintf "no prefix parse function for %s found" p.curToken.Literal)
            None

    let parseStatement (p: ParserState) =
        match p.curToken.Token with 
        //| Token.LET -> parseLetStatement p
        //| TokenType.RETURN -> parseReturnStatement p
        | _ -> parseExpression p

    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer

        //register prefix parse functions
        let prefixFns = new System.Collections.Generic.Dictionary<Token, prefixParse>()
        //prefixFns.Add(TokenType.IDENT, parseIdentifier)
        prefixFns.Add(Token.NUMBER, parseIntegerLiteral)
        //prefixFns.Add(TokenType.BANG, parsePrefixExpression)
        //prefixFns.Add(TokenType.MINUS, parsePrefixExpression)
        //prefixFns.Add(TokenType.TRUE, parseBoolean)
        //prefixFns.Add(TokenType.FALSE, parseBoolean)
        //prefixFns.Add(TokenType.LPAREN, parseGroupedExpression)
        //prefixFns.Add(TokenType.IF, parseIfExpression)
        //prefixFns.Add(TokenType.FUNCTION, parseFunctionLiteral)
        //prefixFns.Add(TokenType.STRING, parseStringLiteral)
        //prefixFns.Add(TokenType.LBRACKET, parseArrayLiteral)
        //prefixFns.Add(TokenType.LBRACE, parseHashLiteral)

        //regist infix parse functions
        let infixFns = new System.Collections.Generic.Dictionary<Token, infixParse>()
        //infixFns.Add(TokenType.PLUS, parseInfixExpression)
        //infixFns.Add(TokenType.MINUS, parseInfixExpression)
        //infixFns.Add(TokenType.SLASH, parseInfixExpression)
        //infixFns.Add(TokenType.ASTERISK, parseInfixExpression)
        //infixFns.Add(TokenType.EQ, parseInfixExpression)
        //infixFns.Add(TokenType.NOT_EQ, parseInfixExpression)
        //infixFns.Add(TokenType.LT, parseInfixExpression)
        //infixFns.Add(TokenType.GT, parseInfixExpression)
        //infixFns.Add(TokenType.LPAREN, parseCallExpression)
        //infixFns.Add(TokenType.LBRACKET, parseIndexExpression)

        let parser = 
            { 
                lexer = lexer
                curToken = firstToken
                peekToken = secondToken
                errors = new ResizeArray<string>()
                prefixParseFns = prefixFns
                infixParseFns = infixFns
            }

        parser

    let parseModule (parser: ParserState) : Ast.Module =
        let expressoinList = new ResizeArray<Ast.Expression>()

        while not (curTokenIs parser Token.EOF) do
            match parseStatement parser with
            | Some statement -> 
                expressoinList.Add(statement)
            | None -> ()

            nextToken parser

        let expressions = expressoinList.ToArray()

        new Ast.Module(expressions)