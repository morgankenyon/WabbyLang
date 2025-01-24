namespace Waux.Lang.Test

module ParserTests =
    open Xunit
    open Waux.Lang
    let isInteger (e : Ast.Expression) =
        match e with
        | :? Ast.IntegerLiteral as id -> true
        | _ -> false
    let canDowncastToInfixExpression (s: Ast.Expression) =
        match s with
        | :? Ast.InfixExpression as ie -> true
        | _ -> false
    let asInfix (s: Ast.Expression) =
        s :?> Ast.InfixExpression

    let testIntegerLiteral (il : Ast.Expression) (value) =
        Assert.True(isInteger il)

        let integerLiteral = il :?> Ast.IntegerLiteral

        Assert.Equal(integerLiteral.value, value)

        Assert.Equal(il.TokenLiteral(), (sprintf "%d" value))
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
    let ``Can parse integer expression`` () =
        let input = "5"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(isInteger(modd.expressions.[0]))

        let literal = modd.expressions.[0] :?> Ast.IntegerLiteral

        Assert.Equal(5, literal.value)// 5L

    [<Fact>]
    let ``Can parse arithmetic expression`` () =
        let input = "2 + 4"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(canDowncastToInfixExpression(modd.expressions.[0]), "cannot downcast to expression")

        let ie = modd.expressions.[0] :?> Ast.InfixExpression

        testIntegerLiteral ie.left 2
        Assert.Equal("+", ie.operator)
        testIntegerLiteral ie.right 4

    [<Fact>]
    let ``Can parse combined arithmetic expression`` () =
        let input = "2 + 4 - 1"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(canDowncastToInfixExpression(modd.expressions.[0]), "cannot downcast to expression")

        let ie = asInfix modd.expressions.[0]

        Assert.True(canDowncastToInfixExpression ie.left)

        let ie2 = asInfix ie.left 

        testIntegerLiteral ie2.left 2
        Assert.Equal("+", ie2.operator)
        testIntegerLiteral ie2.right 4

        Assert.Equal("-", ie.operator)
        testIntegerLiteral ie.right 1

    [<Fact>]
    let ``Can parse multiplication expression`` () =
        let input = "2 * 4"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(canDowncastToInfixExpression(modd.expressions.[0]), "cannot downcast to expression")

        let ie = modd.expressions.[0] :?> Ast.InfixExpression

        testIntegerLiteral ie.left 2
        Assert.Equal("*", ie.operator)
        testIntegerLiteral ie.right 4

    [<Fact>]
    let ``Can parse division expression`` () =
        let input = "4 / 2"

        let lexer = Lexer.createLexer input
        let parser = Parser.createParser lexer
        let modd = Parser.parseModule parser

        AssertNoParseErrors parser

        Assert.Equal(1, modd.expressions.Length)

        Assert.True(canDowncastToInfixExpression(modd.expressions.[0]), "cannot downcast to expression")

        let ie = modd.expressions.[0] :?> Ast.InfixExpression

        testIntegerLiteral ie.left 4
        Assert.Equal("/", ie.operator)
        testIntegerLiteral ie.right 2


