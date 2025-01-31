namespace Waux.Lang

module Parser =
    open Models

    type ExprPrecedence =
        | LOWEST = 1
        | AND_OR = 2
        | EQUALS = 3
        | LESSGREATER = 4
        | SUM = 5
        | PRODUCT = 6
        | PREFIX = 7
        | CALL = 8
        | INDEX = 9

    let PrecedenceMap =
        Map
            .empty
            .Add(Token.EQ, ExprPrecedence.EQUALS)
            .Add(Token.NOT_EQ, ExprPrecedence.EQUALS)
            .Add(Token.LT, ExprPrecedence.LESSGREATER)
            .Add(Token.LT_EQ, ExprPrecedence.LESSGREATER)
            .Add(Token.GT, ExprPrecedence.LESSGREATER)
            .Add(Token.GT_EQ, ExprPrecedence.LESSGREATER)
            .Add(Token.AND, ExprPrecedence.AND_OR)
            .Add(Token.OR, ExprPrecedence.AND_OR)
            .Add(Token.PLUS, ExprPrecedence.SUM)
            .Add(Token.MINUS, ExprPrecedence.SUM)
            .Add(Token.SLASH, ExprPrecedence.PRODUCT)
            .Add(Token.ASTERISK, ExprPrecedence.PRODUCT)
            .Add(Token.LPAREN, ExprPrecedence.CALL)
    //.Add(TokenType.LBRACKET, ExprPrecedence.INDEX)

    type prefixParse = ParserState -> Ast.Expression option
    and infixParse = ParserState -> Ast.Expression -> Ast.Expression option

    and ParserState =
        { lexer: Models.LexerState
          mutable curToken: TokenPair
          mutable peekToken: TokenPair
          mutable errors: ResizeArray<string> //TODO - maybe change to option type??
          prefixParseFns: System.Collections.Generic.Dictionary<Token, prefixParse>
          infixParseFns: System.Collections.Generic.Dictionary<Token, infixParse> }

    let toExpr exp = exp :> Ast.Expression

    let toSome (exp: Ast.Expression) = Some exp

    let toSomeExpr exp = exp |> toExpr |> toSome

    let curTokenIs (p: ParserState) (t: Token) = p.curToken.Token = t

    let peekTokenIs (p: ParserState) (t: Token) = p.peekToken.Token = t

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

    let peekError (p: ParserState) (t: Token) =
        let msg =
            sprintf "expected next token to be %s, got %s instead" (t.ToString()) (p.peekToken.Token.ToString())

        p.errors.Add(msg)

    let getTokenPrecedence tokenType =
        match PrecedenceMap.ContainsKey tokenType with
        | true -> PrecedenceMap.[tokenType]
        //precedence
        | false -> ExprPrecedence.LOWEST

    let peekPrecedence p = getTokenPrecedence p.peekToken.Token

    let expectPeek (p: ParserState) (t: Token) =
        match peekTokenIs p t with
        | true ->
            nextToken p
            true
        | false ->
            peekError p t
            false

    let curPrecedence p = getTokenPrecedence p.curToken.Token

    let parseExpression p precedence =
        match p.prefixParseFns.ContainsKey p.curToken.Token with
        | true ->
            let prefix = p.prefixParseFns.[p.curToken.Token]
            let mutable leftExp = prefix p

            while not (peekTokenIs p Token.SEMICOLON)
                  && precedence < peekPrecedence p do

                leftExp <-
                    match p.infixParseFns.ContainsKey p.peekToken.Token
                          && leftExp.IsSome
                        with
                    | true ->
                        let infixFn = p.infixParseFns.[p.peekToken.Token]
                        nextToken p
                        infixFn p leftExp.Value
                    | _ -> leftExp

            leftExp
        | false ->
            p.errors.Add(sprintf "no prefix parse function for %s found" p.curToken.Literal)
            None

    let parseLetStatement (p: ParserState) =
        let letToken = p.curToken

        if not (expectPeek p Token.IDENT) then
            None
        else
            let identStatement = new Ast.Identifier(p.curToken, p.curToken.Literal)

            if not (expectPeek p Token.ASSIGN) then
                None
            else
                nextToken p

                let value = parseExpression p ExprPrecedence.LOWEST

                while not (curTokenIs p Token.SEMICOLON)
                      && not (curTokenIs p Token.EOF) do
                    nextToken p

                if curTokenIs p Token.EOF then
                    p.errors.Add(
                        sprintf "Let statement identified as \"%s\" needs an ending semicolon" identStatement.value
                    )

                    None
                else
                    match value with
                    | Some v ->
                        let letStatement = new Ast.LetStatement(letToken, identStatement, v)
                        Some(letStatement :> Ast.Statement)
                    | None -> None

    let parseFunctionParameters p =
        let identifiers = new ResizeArray<Ast.Identifier>()

        if peekTokenIs p Token.RPAREN then
            nextToken p
            identifiers.ToArray()
        else
            nextToken p

            let ident = new Ast.Identifier(p.curToken, p.curToken.Literal)
            identifiers.Add(ident)

            while peekTokenIs p Token.COMMA do
                nextToken p
                nextToken p
                let iden = new Ast.Identifier(p.curToken, p.curToken.Literal)
                identifiers.Add(iden)

            if not (expectPeek p Token.RPAREN) then
                Array.empty<Ast.Identifier>
            else
                identifiers.ToArray()



    let parseGroupedExpression p =
        nextToken p

        match parseExpression p ExprPrecedence.LOWEST with
        | Some expression ->
            if not (expectPeek p Token.RPAREN) then
                None
            else
                Some expression
        | None -> None

    let parseIdentifier p =
        new Ast.Identifier(p.curToken, p.curToken.Literal)
        |> toSomeExpr

    let parseInfixExpression p left =
        let curToken = p.curToken

        let precedence = curPrecedence p

        nextToken p

        match parseExpression p precedence with
        | Some right ->
            new Ast.InfixExpression(curToken, left, curToken.Literal, right)
            |> toSomeExpr
        | None -> None

    let parseExpressionStatement p =
        let curToken = p.curToken

        match parseExpression p ExprPrecedence.LOWEST with
        | Some expression ->
            let statement = new Ast.ExpressionStatement(curToken, expression)

            if peekTokenIs p Token.SEMICOLON then
                nextToken p

            Some(statement :> Ast.Statement)
        | None -> None


    let rec parseStatement (p: ParserState) =
        match p.curToken.Token with
        | Token.FUNC -> parseFuncStatement p
        | Token.LET -> parseLetStatement p
        //| TokenType.RETURN -> parseReturnStatement p
        | _ -> parseExpressionStatement p

    and parseFuncStatement (p: ParserState) =
        let curToken = p.curToken

        if not (expectPeek p Token.IDENT) then
            None
        else
            let identExpression = new Ast.Identifier(p.curToken, p.curToken.Literal)

            if not (expectPeek p Token.LPAREN) then
                None
            else

                let parameters = parseFunctionParameters p

                if not (expectPeek p Token.LBRACE) then
                    None
                else
                    let body = parseBlockStatement p

                    let functionStatement =
                        new Ast.FunctionStatement(curToken, identExpression, parameters, body)

                    Some(functionStatement :> Ast.Statement)

    and parseBlockStatement p =

        let curToken = p.curToken

        let statements = new ResizeArray<Ast.Statement>()

        nextToken p

        while not (curTokenIs p Token.RBRACE)
              && not (curTokenIs p Token.EOF) do
            match parseStatement p with
            | Some statement -> statements.Add(statement)
            | None -> ()

            nextToken p

        let statementArray = statements.ToArray()

        let block = new Ast.BlockStatement(curToken, statementArray)

        block

    let parseIfExpression p =
        let curToken = p.curToken

        if not (expectPeek p Token.LPAREN) then
            None
        else
            nextToken p

            let condition = parseExpression p ExprPrecedence.LOWEST

            let notRParen = not (expectPeek p Token.RPAREN)
            let notLBrace = not (expectPeek p Token.LBRACE)

            if notRParen || notLBrace || condition.IsNone then
                None
            else
                let consequence = parseBlockStatement p

                if peekTokenIs p Token.ELSE then
                    nextToken p

                    if not (expectPeek p Token.LBRACE) then
                        None
                    else
                        let alternative = parseBlockStatement p

                        new Ast.IfElseExpression(curToken, condition.Value, consequence, Some alternative)
                        |> toSomeExpr
                else
                    new Ast.IfElseExpression(curToken, condition.Value, consequence, None)
                    |> toSomeExpr

    let parseExpressionList p endToken =
        let exprList = new ResizeArray<Ast.Expression>()

        if peekTokenIs p endToken then
            nextToken p
            Some(exprList.ToArray())
        else
            nextToken p

            let expr = parseExpression p ExprPrecedence.LOWEST

            if expr.IsSome then
                exprList.Add(expr.Value)
                ()
            else
                ()

            while peekTokenIs p Token.COMMA do
                nextToken p
                nextToken p


                let nextExpr = parseExpression p ExprPrecedence.LOWEST

                if nextExpr.IsSome then
                    exprList.Add(nextExpr.Value)
                    ()
                else
                    ()

            if not (expectPeek p endToken) then
                None
            else
                Some(exprList.ToArray())

    let parseCallExpression p (func: Ast.Expression) =
        let curToken = p.curToken

        match parseExpressionList p Token.RPAREN with
        | Some arguments ->
            new Ast.CallExpression(curToken, func, arguments)
            |> toSomeExpr
        | None -> None

    //let parseFunctionParameters p =
    //    let identifiers = new ResizeArray<Ast.Identifier>()

    //    if peekTokenIs p Token.RPAREN then
    //        nextToken p
    //        identifiers.ToArray()
    //    else
    //        nextToken p

    //        let ident = new Ast.Identifier(p.curToken, p.curToken.Literal)
    //        identifiers.Add(ident)

    //        while peekTokenIs p Token.COMMA do
    //            nextToken p
    //            nextToken p
    //            let iden = new Ast.Identifier(p.curToken, p.curToken.Literal)
    //            identifiers.Add(iden)

    //        if not (expectPeek p Token.RPAREN) then
    //            Array.empty<Ast.Identifier>
    //        else identifiers.ToArray()


    //let parseFunctionLiteral p =
    //    let curToken = p.curToken

    //    if not (expectPeek p Token.LPAREN) then
    //        None
    //    else

    //        let parameters = parseFunctionParameters p

    //        if not (expectPeek p Token.LBRACE) then
    //            None
    //        else
    //            let body = parseBlockStatement p

    //            new Ast.FunctionStatement(curToken, parameters, body)
    //            |> toSomeExpr



    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer

        //register prefix parse functions
        let prefixFns = new System.Collections.Generic.Dictionary<Token, prefixParse>()
        prefixFns.Add(Token.IDENT, parseIdentifier)
        prefixFns.Add(Token.NUMBER, parseIntegerLiteral)
        //prefixFns.Add(TokenType.BANG, parsePrefixExpression)
        //prefixFns.Add(TokenType.MINUS, parsePrefixExpression)
        //prefixFns.Add(TokenType.TRUE, parseBoolean)
        //prefixFns.Add(TokenType.FALSE, parseBoolean)
        prefixFns.Add(Token.LPAREN, parseGroupedExpression)
        prefixFns.Add(Token.IF, parseIfExpression)
        //prefixFns.Add(Token.FUNC, parseFunctionLiteral)
        //prefixFns.Add(TokenType.STRING, parseStringLiteral)
        //prefixFns.Add(TokenType.LBRACKET, parseArrayLiteral)
        //prefixFns.Add(TokenType.LBRACE, parseHashLiteral)

        //register infix parse functions
        let infixFns = new System.Collections.Generic.Dictionary<Token, infixParse>()
        infixFns.Add(Token.PLUS, parseInfixExpression)
        infixFns.Add(Token.MINUS, parseInfixExpression)
        infixFns.Add(Token.SLASH, parseInfixExpression)
        infixFns.Add(Token.ASTERISK, parseInfixExpression)
        infixFns.Add(Token.EQ, parseInfixExpression)
        infixFns.Add(Token.NOT_EQ, parseInfixExpression)
        infixFns.Add(Token.LT, parseInfixExpression)
        infixFns.Add(Token.LT_EQ, parseInfixExpression)
        infixFns.Add(Token.GT, parseInfixExpression)
        infixFns.Add(Token.GT_EQ, parseInfixExpression)
        infixFns.Add(Token.AND, parseInfixExpression)
        infixFns.Add(Token.OR, parseInfixExpression)
        infixFns.Add(Token.LPAREN, parseCallExpression)
        //infixFns.Add(TokenType.LBRACKET, parseIndexExpression)

        let parser =
            { lexer = lexer
              curToken = firstToken
              peekToken = secondToken
              errors = new ResizeArray<string>()
              prefixParseFns = prefixFns
              infixParseFns = infixFns }

        parser

    let parseModule (parser: ParserState) : Ast.Module =
        let statementsList = new ResizeArray<Ast.Statement>()

        while not (curTokenIs parser Token.EOF) do
            match parseStatement parser with
            | Some statement -> statementsList.Add(statement)
            | None -> ()

            nextToken parser

        let statements = statementsList.ToArray()

        new Ast.Module(statements)
