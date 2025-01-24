namespace Waux.Lang

module Parser =
    open Models

    type ExprPrecedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    | INDEX = 8

    let PrecedenceMap =
        Map.empty
            //.Add(Token.EQ, ExprPrecedence.EQUALS)
            //.Add(TokenType.NOT_EQ, ExprPrecedence.EQUALS)
            //.Add(TokenType.LT, ExprPrecedence.LESSGREATER)
            //.Add(TokenType.GT, ExprPrecedence.LESSGREATER)
            .Add(Token.PLUS, ExprPrecedence.SUM)
            .Add(Token.MINUS, ExprPrecedence.SUM)
            .Add(Token.SLASH, ExprPrecedence.PRODUCT)
            .Add(Token.ASTERISK, ExprPrecedence.PRODUCT)
            //.Add(TokenType.LPAREN, ExprPrecedence.CALL)
            //.Add(TokenType.LBRACKET, ExprPrecedence.INDEX)

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

    let peekTokenIs (p: ParserState) (t: Token) =
        p.peekToken.Token = t

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

    let getTokenPrecedence tokenType =
        match PrecedenceMap.ContainsKey tokenType with
        | true ->
            PrecedenceMap.[tokenType]
            //precedence
        | false -> 
            ExprPrecedence.LOWEST

    let peekPrecedence p =
        getTokenPrecedence p.peekToken.Token

    let curPrecedence p =
        getTokenPrecedence p.curToken.Token
    let parseExpression p precedence =
        match p.prefixParseFns.ContainsKey p.curToken.Token with
        | true -> 
            let prefix = p.prefixParseFns.[p.curToken.Token]
            let mutable leftExp = prefix p

            while not (peekTokenIs p Token.SEMICOLON) && precedence < peekPrecedence p do
                
                leftExp <- match p.infixParseFns.ContainsKey p.peekToken.Token && leftExp.IsSome with
                            | true ->
                                let infixFn = p.infixParseFns.[p.peekToken.Token]
                                nextToken p
                                infixFn p leftExp.Value
                            | _ -> leftExp
            leftExp
        | false -> 
            p.errors.Add(sprintf "no prefix parse function for %s found" p.curToken.Literal)
            None

    let parseInfixExpression p left =
        let curToken = p.curToken

        let precedence = curPrecedence p

        nextToken p

        match parseExpression p precedence with
        | Some right ->
            new Ast.InfixExpression(curToken, left, curToken.Literal, right)
            |> toSomeExpr
        | None -> None

    let parseStatement (p: ParserState) =
        match p.curToken.Token with 
        //| Token.LET -> parseLetStatement p
        //| TokenType.RETURN -> parseReturnStatement p
        | _ -> parseExpression p ExprPrecedence.LOWEST

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

        //register infix parse functions
        let infixFns = new System.Collections.Generic.Dictionary<Token, infixParse>()
        infixFns.Add(Token.PLUS, parseInfixExpression)
        infixFns.Add(Token.MINUS, parseInfixExpression)
        infixFns.Add(Token.SLASH, parseInfixExpression)
        infixFns.Add(Token.ASTERISK, parseInfixExpression)
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