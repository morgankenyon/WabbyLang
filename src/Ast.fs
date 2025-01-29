namespace Waux.Lang

module Ast =
    open Models

    type StatementType =
        | Module
        | ExpressionStatement
        | LetStatement
        | BlockStatement
        | FunctionStatement

    type ExpressionType =
        | InfixExpression
        | IntegerLiteral
        | Identifier
        | CallExpression
        | IfElseExpression


    //type AstType =
    //| Module
    //| InfixExpression
    //| IntegerLiteral
    //| ExpressionStatement
    //| Identifier
    //| LetStatement

    //| LongLiteral

    type NodeType =
        | Statement
        | Expression

    type Node =
        abstract member NodeType: NodeType
        abstract member TokenLiteral: unit -> string
        abstract member Str: unit -> string
    //abstract member AType : unit -> AstType

    type Statement =
        inherit Node
        abstract member StateType: unit -> StatementType

    type Expression =
        inherit Node
        abstract member ExprType: unit -> ExpressionType

    type Module(statements: Statement []) =
        member this.statements = statements

        interface Statement with
            member this.NodeType = NodeType.Statement
            member this.StateType() = StatementType.Module

            member this.TokenLiteral() =
                match this.statements.Length with
                | 0 -> ""
                | _ ->
                    let firstStatement = this.statements.[0]
                    (firstStatement :> Node).TokenLiteral()

            member this.Str() =
                this.statements
                |> Array.map (fun s -> s.Str())
                |> Array.reduce (fun a b -> a + b)

    type Identifier(token: TokenPair, value: string) =
        member this.token = token
        member this.value = value

        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.ExprType() = ExpressionType.Identifier
            member this.TokenLiteral() = token.Literal
            member this.Str() = value

    type BlockStatement(token: TokenPair, statements: Statement []) =
        member this.token = token
        member this.statements = statements

        interface Statement with
            member this.NodeType = NodeType.Statement
            member this.StateType() = StatementType.BlockStatement
            member this.TokenLiteral() = this.token.Literal

            member this.Str() =
                this.statements
                |> Array.map (fun s -> s.Str())
                |> Array.reduce (fun a b -> a + b)

    type FunctionStatement(token: TokenPair, name: Identifier, parameters: Identifier [], body: BlockStatement) =
        member this.token = token
        member this.name = name
        member this.parameters = parameters
        member this.body = body

        interface Statement with
            member this.NodeType = NodeType.Statement
            member this.StateType() = StatementType.FunctionStatement
            member this.TokenLiteral() = this.token.Literal

            member this.Str() =
                let paraStr =
                    this.parameters
                    |> Array.map (fun s -> (s :> Expression).Str())
                    |> Array.reduce (fun a b -> sprintf "%s, %s" a b)

                let bodyStr = (this.body :> Statement).Str()
                sprintf "%s (%s) %s" this.token.Literal paraStr bodyStr

    type IfElseExpression
        (
            token: TokenPair,
            condition: Expression,
            consequence: BlockStatement,
            alternative: BlockStatement option
        ) =
        member this.token = token
        member this.condition = condition
        member this.consequence = consequence
        member this.alternative = alternative

        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.TokenLiteral() = this.token.Literal
            member this.ExprType() = ExpressionType.IfElseExpression

            member this.Str() =
                let ifStr = this.condition.Str()
                let consequenceStr = (this.consequence :> Statement).Str()

                let firstStr = sprintf "if%s %s" ifStr consequenceStr

                match this.alternative with
                | Some alt ->
                    let altStr = (alt :> Statement).Str()
                    sprintf "%selse %s" firstStr altStr
                | None -> firstStr

    type LetStatement(token: TokenPair, name: Identifier, value: Expression) =
        member this.token = token
        member this.name = name
        member this.value = value

        interface Statement with
            member this.NodeType = NodeType.Statement
            member this.StateType() = StatementType.LetStatement
            member this.TokenLiteral() = token.Literal

            member this.Str() =
                sprintf "%s %s = %s;" (this.token.Literal) ((this.name :> Expression).Str()) (this.value.Str())

    type ExpressionStatement(token: TokenPair, expression: Expression) =
        member this.token = token
        member this.expression = expression

        interface Statement with
            member this.NodeType = NodeType.Statement
            member this.StateType() = StatementType.ExpressionStatement
            member this.TokenLiteral() = token.Literal
            member this.Str() = this.expression.Str()

    type CallExpression(token: TokenPair, func: Expression, arguments: Expression []) =
        member this.token = token
        member this.func = func
        member this.arguments = arguments
        member this.funcName = this.func.Str()

        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.ExprType() = ExpressionType.CallExpression
            member this.TokenLiteral() = this.token.Literal

            member this.Str() =
                let argStr =
                    this.arguments
                    |> Array.map (fun s -> s.Str())
                    |> Array.reduce (fun a b -> sprintf "%s, %s" a b)

                let funcStr = this.func.Str()

                sprintf "%s(%s)" funcStr argStr

    type InfixExpression(token: TokenPair, left: Expression, operator: string, right: Expression) =
        member this.token = token
        member this.left = left
        member this.operator = operator
        member this.right = right

        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.ExprType() = ExpressionType.InfixExpression
            member this.TokenLiteral() = token.Literal

            member this.Str() =
                let leftStr = this.left.Str()
                let rightStr = this.right.Str()

                sprintf "(%s %s %s)" leftStr this.operator rightStr

    type IntegerLiteral(token: TokenPair, value: int32) =
        member this.token = token
        member this.value = value

        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.ExprType() = ExpressionType.IntegerLiteral
            member this.TokenLiteral() = token.Literal
            //member this.expressionNode () = ()
            member this.Str() = sprintf "%d" value

        override x.Equals(y) =
            match y with
            | :? IntegerLiteral as y -> (x = y)
            | _ -> false

        override this.GetHashCode() = this.value.GetHashCode()

        interface System.IComparable with
            member x.CompareTo y =
                match y with
                | :? IntegerLiteral as y -> x.value.CompareTo(y.value)
                | _ -> invalidArg "y" "cannot compare values of different types"

//type LongLiteral(token: TokenPair, value: int64) =
//    member this.token = token
//    member this.value = value
//    interface Expression with
//        member this.AType () = AstType.IntegerLiteral
//        member this.TokenLiteral () = token.Literal
//        //member this.expressionNode () = ()
//        member this.Str () = sprintf "%d" value
//    override x.Equals(y) =
//        match y with
//            | :? LongLiteral as y -> (x = y)
//            | _ -> false
//    override this.GetHashCode() =
//        this.value.GetHashCode()
//    interface System.IComparable with
//        member x.CompareTo y =
//            match y with
//                | :? LongLiteral as y -> x.value.CompareTo(y.value)
//                | _ -> invalidArg "y" "cannot compare values of different types"
