namespace Waux.Lang

module Ast =
    open Models
    type AstType =
        | Module
        | InfixExpression
        | IntegerLiteral

        //| LongLiteral

    type NodeType =
        | Statement
        | Expression

    type Node =
        abstract member NodeType : NodeType
        abstract member TokenLiteral : unit -> string
        abstract member Str : unit -> string
        abstract member AType : unit -> AstType

    type Statement =
        inherit Node

    type Expression =
        inherit Node

    type Module(expressions : Expression []) =
        member this.expressions = expressions
        interface Statement with 
            member this.NodeType = NodeType.Statement
            member this.AType () = AstType.Module
            member this.TokenLiteral () =
                match this.expressions.Length with 
                | 0 -> ""
                | _ -> 
                    let firstStatement = this.expressions.[0]
                    (firstStatement :> Node).TokenLiteral()
            member this.Str () =
                this.expressions
                    |> Array.map (fun s -> s.Str())
                    |> Array.reduce (fun a b -> a + b)

    type InfixExpression(token: TokenPair, left: Expression, operator: string, right: Expression) =
        member this.token = token
        member this.left = left
        member this.operator = operator
        member this.right = right
        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.AType () = AstType.InfixExpression
            member this.TokenLiteral () = token.Literal
            member this.Str () = 
                let leftStr = this.left.Str()
                let rightStr = this.right.Str()

                sprintf "(%s %s %s)"  leftStr this.operator rightStr

    type IntegerLiteral(token: TokenPair, value: int32) =
        member this.token = token
        member this.value = value
        interface Expression with
            member this.NodeType = NodeType.Expression
            member this.AType () = AstType.IntegerLiteral
            member this.TokenLiteral () = token.Literal
            //member this.expressionNode () = ()
            member this.Str () = sprintf "%d" value
        override x.Equals(y) =
            match y with
                | :? IntegerLiteral as y -> (x = y)
                | _ -> false
        override this.GetHashCode() = 
            this.value.GetHashCode()
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
