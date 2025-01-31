namespace Waux.Lang

module Wasm =
    open System
    open System.Collections.Generic

    [<Literal>]
    let SECTION_ID_TYPE = 1uy

    [<Literal>]
    let SECTION_ID_FUNCTION = 3uy

    [<Literal>]
    let SECTION_ID_EXPORT = 7uy

    [<Literal>]
    let SECTION_ID_CODE = 10uy

    [<Literal>]
    let TYPE_FUNCTION = 96uy

    [<Literal>]
    let INSTR_BLOCK = 2uy

    [<Literal>]
    let INSTR_LOOP = 3uy

    [<Literal>]
    let INSTR_BR = 12uy

    [<Literal>]
    let INSTR_BR_IF = 13uy

    [<Literal>]
    let INSTR_IF = 4uy

    [<Literal>]
    let INSTR_ELSE = 5uy

    [<Literal>]
    let i32_VAL_TYPE = 127uy

    [<Literal>]
    let INSTR_i32_CONST = 65uy

    [<Literal>]
    let INSTR_i32_ADD = 106uy

    [<Literal>]
    let INSTR_i32_SUB = 107uy

    [<Literal>]
    let INSTR_i32_MUL = 108uy

    [<Literal>]
    let INSTR_i32_DIV_S = 109uy

    [<Literal>]
    let i64_VAL_TYPE = 126uy

    [<Literal>]
    let INSTR_i64_CONST = 66uy

    [<Literal>]
    let f32_VAL_TYPE = 125uy

    [<Literal>]
    let INSTR_f32_CONST = 67uy

    [<Literal>]
    let f64_VAL_TYPE = 124uy

    [<Literal>]
    let INSTR_f64_CONST = 68uy

    [<Literal>]
    let INSTR_LOCAL_GET = 32uy

    [<Literal>]
    let INSTR_LOCAL_SET = 33uy

    [<Literal>]
    let INSTR_LOCAL_TEE = 34uy

    [<Literal>]
    let INSTR_END = 11uy

    [<Literal>]
    let INSTR_CALL = 16uy

    [<Literal>]
    let INSTR_DROP = 26uy

    [<Literal>]
    let EMPTY = 64uy

    [<Literal>]
    let INSTR_i32_EQZ = 69uy

    [<Literal>]
    let INSTR_i32_EQ = 70uy

    [<Literal>]
    let INSTR_i32_NE = 71uy

    [<Literal>]
    let INSTR_i32_LT_S = 72uy

    [<Literal>]
    let INSTR_i32_LT_U = 73uy

    [<Literal>]
    let INSTR_i32_GT_S = 74uy

    [<Literal>]
    let INSTR_i32_GT_U = 75uy

    [<Literal>]
    let INSTR_i32_LE_S = 76uy

    [<Literal>]
    let INSTR_i32_LE_U = 77uy

    [<Literal>]
    let INSTR_i32_GE_S = 78uy

    [<Literal>]
    let INSTR_i32_GE_U = 79uy

    [<Literal>]
    let INSTR_i32_AND = 113uy

    [<Literal>]
    let INSTR_i32_OR = 114uy

    type WasmValueBytes =
        | Value of byte
        | Values of byte array

    type WasmTree =
        | Empty
        | Node of value: WasmValueBytes array option * inners: WasmTree array option

    type WasmFuncBytes =
        { name: string
          paramTypes: byte array
          resultType: byte
          locals: byte array array
          body: byte array }

    type SymbolType =
        | Local
        | Param

    type SymbolEntry =
        { name: string
          index: int
          symbolType: SymbolType }

    type SymbolDict = System.Collections.Generic.Dictionary<string, SymbolEntry>
    type NestedSymbolDict = Dictionary<string, SymbolDict * int>

    type SymbolEntries =
        | Nested of NestedSymbolDict
        | Locals of SymbolDict

    type SymbolScope = System.Collections.Generic.LinkedList<SymbolEntries>

    let stringToBytes (s: string) = System.Text.Encoding.UTF8.GetBytes(s)

    let int32ToBytes (v: int32) =
        let bytes = BitConverter.GetBytes(v)

        match BitConverter.IsLittleEndian with
        | true -> bytes
        | false -> Array.rev bytes

    let magic () =
        // [0x00, 0x61, 0x73, 0x6d]
        let nullChar = Convert.ToChar(0).ToString()
        stringToBytes ($"{nullChar}asm")

    let version () =
        // [0x01, 0x00, 0x00, 0x00]
        int32ToBytes (1)

    let u32 (v: uint) = if v <= 127u then byte v else 0uy

    let i32 (v: int) = if v <= 63 then byte v else 0uy

    let locals (n: int32) (b: byte) = [| i32 n; b |]

    let section (id: byte) (contents: byte array) =
        let normalizedSize = i32 contents.Length
        let headers = [| id; normalizedSize |]
        Array.concat [ headers; contents ]

    let vec (elements: byte array) =
        let normalizedSize = i32 elements.Length

        Array.concat [ [| normalizedSize |]
                       elements ]

    let vecFlatten (elements: byte array array) =
        let nonEmptyElements =
            elements
            |> Array.filter (fun ele -> ele.Length > 0)

        let normalizedSize = i32 nonEmptyElements.Length
        let flattenedElements = elements |> Array.collect id

        Array.concat [ [| normalizedSize |]
                       flattenedElements ]

    //Type Section
    let functype (paramTypes: byte array, resultTypes: byte array) =
        let paramVec = vec paramTypes
        let resultVec = vec resultTypes

        Array.concat [ [| TYPE_FUNCTION |]
                       paramVec
                       resultVec ]

    let typesec (functypes: byte array array) =
        let funcVec = vecFlatten functypes
        section SECTION_ID_TYPE funcVec

    //Function Section
    let funcsec (typeidxs: byte array array) =
        vecFlatten typeidxs |> section SECTION_ID_FUNCTION

    //Export section
    let exportdesc (idx: byte) = [| 0uy; idx |]
    let name (s: string) = s |> stringToBytes |> vec

    let export (s: string) (exportDesc: byte array) = Array.concat [ name (s); exportDesc ]

    let exportsec (exports: byte array array) =
        vecFlatten exports |> section SECTION_ID_EXPORT

    //Code Section
    let func (locals: byte array) (body: byte array) =
        let localsVec = vec locals
        Array.concat [ localsVec; body ]

    let funcNested (locals: byte array array) (body: byte array) =
        let localsVec = vecFlatten locals
        Array.concat [ localsVec; body ]

    let code (func: byte array) =
        let normalizedSize = i32 func.Length

        Array.concat [ [| normalizedSize |]
                       func ]

    let codesec (codes: byte array array) =
        vecFlatten codes |> section SECTION_ID_CODE

    let modd (sections: byte array array) =
        let flattenedSections = sections |> Array.collect id

        Array.concat [ magic ()
                       version ()
                       flattenedSections ]

    type blockType =
        | Empty_
        | I32
        | I64
        | F32
        | F64

    let getBlockType bType =
        match bType with
        | Empty_ -> EMPTY
        | I32 -> i32_VAL_TYPE
        | I64 -> i64_VAL_TYPE
        | F32 -> f32_VAL_TYPE
        | F64 -> f64_VAL_TYPE

    let getOperator op =
        match op with
        //arithmetic
        | "+" -> INSTR_i32_ADD
        | "-" -> INSTR_i32_SUB
        | "*" -> INSTR_i32_MUL
        | "/" -> INSTR_i32_DIV_S
        //comparison
        | "==" -> INSTR_i32_EQ
        | "!=" -> INSTR_i32_NE
        | "<" -> INSTR_i32_LT_S
        | "<=" -> INSTR_i32_LE_S
        | ">" -> INSTR_i32_GT_S
        | ">=" -> INSTR_i32_GE_S
        //logic
        | "and" -> INSTR_i32_AND
        | "or" -> INSTR_i32_OR
        //extra
        | _ -> INSTR_END

    let resolveSymbols (symbolMap: SymbolDict) (name: string) =
        let containsKey = symbolMap.ContainsKey name

        match containsKey with
        | true ->
            let symbol = symbolMap[name]
            Ok symbol
        | false -> Error $"Error: undeclared identifier: {name}"

    let rec private expressionToWasmTree
        (expr: Ast.Expression)
        (symbols: NestedSymbolDict)
        (symbolMap: SymbolDict)
        : WasmTree =
        match expr.ExprType() with
        | Ast.ExpressionType.IntegerLiteral ->
            let integerLiteral = expr :?> Ast.IntegerLiteral
            let value = i32 integerLiteral.value
            let wasmBytes = [| INSTR_i32_CONST; value |]
            let node = Node(Some [| Values wasmBytes |], None)
            node
        | Ast.ExpressionType.InfixExpression ->
            let infixExpression = expr :?> Ast.InfixExpression
            let leftValue = expressionToWasmTree infixExpression.left symbols symbolMap
            let operator = getOperator infixExpression.operator
            let operatorValue = Node(Some [| Value operator |], None)
            let rightValue = expressionToWasmTree infixExpression.right symbols symbolMap

            let inner =
                [| leftValue
                   rightValue
                   operatorValue |]

            let node = Node(None, Some inner)
            node
        | Ast.ExpressionType.Identifier ->
            let iden = expr :?> Ast.Identifier

            let symbol = resolveSymbols symbolMap iden.value

            match symbol with
            | Ok sy ->
                let wasmBytes = [| INSTR_LOCAL_GET; i32 sy.index |]
                let node = Node(Some [| Values wasmBytes |], None)
                node
            | Error msg -> raise (Exception(msg))
        | Ast.ExpressionType.CallExpression ->
            let callExpr = expr :?> Ast.CallExpression

            let name = callExpr.funcName

            let index =
                if symbols.ContainsKey name then
                    let (_, idx) = symbols[name]
                    idx
                else
                    raise (new Exception("Cannot find function name in symbols table"))

            let mutable arguTrees: WasmTree array = [||]

            for args in callExpr.arguments do
                let argTree = expressionToWasmTree args symbols symbolMap

                arguTrees <-
                    Array.concat [ arguTrees
                                   [| argTree |] ]

            let wasmBytes = [| INSTR_CALL; i32 index |]
            let node = Node(Some [| Values wasmBytes |], Some arguTrees)
            node
        | Ast.ExpressionType.IfElseExpression ->
            let ifElseExpr = expr :?> Ast.IfElseExpression

            let exprWasm = expressionToWasmTree ifElseExpr.condition symbols symbolMap
            let ifCommandWasm = Node(Some [| Values [| INSTR_IF; getBlockType I32 |] |], None)
            let thenBlockWasm = statementToWasmTree ifElseExpr.consequence symbols symbolMap
            let elseCommandWasm = Node(Some [| Values [| INSTR_ELSE |] |], None)

            let elseBlockWasm =
                statementToWasmTree ifElseExpr.alternative.Value symbols symbolMap

            let endingWasm = Node(Some [| Values [| INSTR_END |] |], None)

            let ifElseWasm =
                [| exprWasm
                   ifCommandWasm
                   thenBlockWasm
                   elseCommandWasm
                   elseBlockWasm
                   endingWasm |]

            let node = Node(None, Some ifElseWasm)
            node
        | Ast.ExpressionType.AssignmentExpression ->
            let assignExpr = expr :?> Ast.AssignmentExpression
            let symbol = resolveSymbols symbolMap assignExpr.name.value

            match symbol with
            | Ok sy ->
                let expreTree = expressionToWasmTree assignExpr.value symbols symbolMap

                let wasmBytes =
                    [| INSTR_LOCAL_TEE
                       i32 sy.index
                       INSTR_DROP |]

                let node = Node(Some [| Values wasmBytes |], Some [| expreTree |])
                node
            | Error msg -> raise (Exception(msg))
        | _ -> Empty

    and private statementToWasmTree
        (state: Ast.Statement)
        (symbols: NestedSymbolDict)
        (symbolMap: SymbolDict)
        : WasmTree =
        match state.StateType() with
        | Ast.StatementType.LetStatement ->
            let letState = state :?> Ast.LetStatement
            let symbol = resolveSymbols symbolMap letState.name.value

            match symbol with
            | Ok sy ->
                let expreTree = expressionToWasmTree letState.value symbols symbolMap
                let wasmBytes = [| INSTR_LOCAL_SET; i32 sy.index |]
                let node = Node(Some [| Values wasmBytes |], Some [| expreTree |])
                node
            | Error msg -> raise (Exception(msg))
        | Ast.StatementType.ExpressionStatement ->
            let exprState = state :?> Ast.ExpressionStatement
            let exprTree = expressionToWasmTree exprState.expression symbols symbolMap
            let node = Node(None, Some [| exprTree |])
            node
        | Ast.StatementType.BlockStatement ->
            let blockState = state :?> Ast.BlockStatement
            let mutable innerTrees: WasmTree array = [||]

            for state in blockState.statements do
                innerTrees <-
                    Array.concat [ innerTrees
                                   [| statementToWasmTree state symbols symbolMap |] ]

            let node = Node(None, Some innerTrees)
            node
        | Ast.StatementType.FunctionStatement ->
            let fn = state :?> Ast.FunctionStatement

            let wasmBytes = statementToWasmTree fn.body symbols symbolMap
            let inner = [| wasmBytes |]
            let node = Node(None, Some inner)
            node
        | Ast.StatementType.WhileStatement ->
            let whileState = state :?> Ast.WhileStatement

            let loopEntry =
                Node(
                    Some [| Values [| INSTR_LOOP
                                      getBlockType blockType.Empty_ |] |],
                    None
                )

            let cond = expressionToWasmTree whileState.condition symbols symbolMap

            let ifEntry =
                Node(
                    Some [| Values [| INSTR_IF
                                      getBlockType Empty_ |] |],
                    None
                )

            let body = statementToWasmTree whileState.body symbols symbolMap

            let brEntry =
                Node(
                    Some [| Values [| INSTR_BR
                                      i32 1
                                      INSTR_END
                                      INSTR_END |] |],
                    None
                )

            let whileWasm =
                [| loopEntry
                   cond
                   ifEntry
                   body
                   brEntry |]

            let node = Node(None, Some whileWasm)
            node
        | _ -> Empty

    let private generateBytes (bytes: byte array) (v: WasmValueBytes) =
        match v with
        | Value vv -> Array.concat [ bytes; [| vv |] ]
        | Values vvs -> Array.concat [ bytes; vvs ]

    let rec private wasmTreeToBytes (tree: WasmTree) : byte array =
        match tree with
        | Empty -> [||]
        | Node (value, inners) when value.IsSome && inners.IsNone ->
            let vals = value.Value
            let mutable bytes: byte array = [||]

            for v in vals do
                bytes <- generateBytes bytes v

            bytes
        | Node (value, inners) when value.IsNone && inners.IsSome ->
            let inn = inners.Value
            let mutable bytes: byte array = [||]

            for v in inn do
                let innerBytes = wasmTreeToBytes v
                bytes <- Array.concat [ bytes; innerBytes ]

            bytes
        | Node (value, inners) when value.IsSome && inners.IsSome ->
            let vals = value.Value
            let inn = inners.Value
            let mutable bytes: byte array = [||]

            for i in inn do
                let innerBytes = wasmTreeToBytes i
                bytes <- Array.concat [ bytes; innerBytes ]

            for v in vals do
                bytes <- generateBytes bytes v

            bytes
        | _ -> [||]

    let rec buildSymbolTable (statement: Ast.Statement) (scopes: SymbolScope) =
        match statement.StateType() with
        | Ast.StatementType.Module ->
            let modd = statement :?> Ast.Module

            for state in modd.statements do
                buildSymbolTable state scopes

            ()
        | Ast.StatementType.FunctionStatement ->
            let funcState = statement :?> Ast.FunctionStatement
            let name = funcState.name.value
            let locals = new SymbolDict()
            let last = scopes.Last.Value

            match last with
            | Nested (inner) -> inner.Add(name, (locals, inner.Count))
            | Locals _ -> raise (new Exception("Should have been a nested entry here instead of locals"))


            scopes.AddLast(Locals locals) |> ignore

            for param in funcState.parameters do
                let paramName = param.value

                let symbolEntry =
                    { name = name
                      index = locals.Count
                      symbolType = SymbolType.Param }

                locals.Add(paramName, symbolEntry)

            buildSymbolTable funcState.body scopes
            scopes.RemoveLast()
            ()
        | Ast.StatementType.LetStatement ->
            let letState = statement :?> Ast.LetStatement
            let name = letState.name.value

            let idx =
                match scopes.Last.Value with
                | Nested inner -> inner.Count
                | Locals locals -> locals.Count

            let symbolEntry =
                {

                  name = name
                  index = idx
                  symbolType = SymbolType.Local }

            match scopes.Last.Value with
            | Nested _ -> raise (new Exception("Should have been a local entry here instead of nested"))
            | Locals locals -> locals.Add(name, symbolEntry)
        | Ast.StatementType.BlockStatement ->
            let block = statement :?> Ast.BlockStatement

            for state in block.statements do
                buildSymbolTable state scopes

            ()

        | _ -> ()

    let buildSymbolMap (codeModule: Ast.Module) =
        let scopes = new SymbolScope()
        let nested = Nested(NestedSymbolDict())
        scopes.AddLast nested |> ignore
        buildSymbolTable codeModule scopes
        scopes

    let rec defineFunctionDecls (statement: Ast.Statement) (symbols: NestedSymbolDict) : WasmFuncBytes array =
        match statement.StateType() with
        | Ast.StatementType.BlockStatement ->
            let block = statement :?> Ast.BlockStatement
            let mutable funcBytes: WasmFuncBytes array = [||]

            for state in block.statements do
                let stateBytes = defineFunctionDecls state symbols
                funcBytes <- Array.concat [ funcBytes; stateBytes ]

            funcBytes

        | Ast.StatementType.FunctionStatement ->
            let funcState = statement :?> Ast.FunctionStatement
            let name = funcState.name.value

            let (funcSymbols, localVars) =
                if symbols.ContainsKey name then
                    let (currentSymbols, _) = symbols[name]
                    let mutable symbolValues: SymbolEntry array = [||]

                    for sym in currentSymbols.Values do
                        symbolValues <- Array.concat [ symbolValues; [| sym |] ]

                    (currentSymbols, symbolValues)
                else
                    (new SymbolDict(), [||])

            let paramVals =
                localVars
                |> Array.filter (fun se -> se.symbolType = SymbolType.Param)

            let paramTypes = paramVals |> Array.map (fun _ -> i32_VAL_TYPE)

            let varsCount =
                localVars
                |> Array.filter (fun se -> se.symbolType = SymbolType.Local)
                |> Array.length

            let bodyWasm =
                statementToWasmTree funcState symbols funcSymbols
                |> wasmTreeToBytes

            let functionDecls: WasmFuncBytes array =
                [| { name = name
                     paramTypes = paramTypes
                     resultType = i32_VAL_TYPE
                     locals = [| locals varsCount i32_VAL_TYPE |]
                     body =
                       Array.concat [ bodyWasm
                                      [| INSTR_END |] ] } |]

            functionDecls
        | _ -> [||]

    let buildModule (functionDecls: WasmFuncBytes array) =
        //creating code section
        let codeSection =
            functionDecls
            |> Array.map (fun f -> funcNested f.locals f.body)
            |> Array.map (fun f -> code f)
            |> codesec

        //Creating type section
        let typeSection =
            functionDecls
            |> Array.map (fun f -> functype (f.paramTypes, [| f.resultType |]))
            |> typesec

        //creating func section
        let funcSection =
            functionDecls
            |> Array.mapi (fun i x -> [| i32 i |])
            |> funcsec

        //creating export section
        let exportSection =
            functionDecls
            |> Array.mapi (fun i f -> export f.name (exportdesc (i32 (i))))
            |> exportsec

        let bytes =
            modd [| typeSection
                    funcSection
                    exportSection
                    codeSection |]

        bytes

    let compile (codeModule: Ast.Module) =
        let symbolScope = buildSymbolMap codeModule

        let symbols =
            match symbolScope.First.Value with
            | Nested nested -> nested
            | Locals _ -> raise (new Exception("Should not be locals"))

        let mutable functionDecls: WasmFuncBytes array = [||]

        for statement in codeModule.statements do
            let funcDecls = defineFunctionDecls statement symbols

            functionDecls <-
                Array.concat [ functionDecls
                               funcDecls ]

        buildModule functionDecls
