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
    let INSTR_i32_MOD_S = 111uy

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

    [<Literal>]
    let SEVEN_BIT_MASK_U : uint32 = 127u
    [<Literal>]
    let SEVEN_BIT_MASK_S : int32 = 127
    [<Literal>]
    let CONTINUATION_BIT : byte = 128uy

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

    let uint32ToBytes (v: uint32) =
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
    [<Literal>]
    let SEVEN_BIT_MASK = 127u

    let u32 (v: uint32) =
        let mutable vall = v
        let mutable r : byte array = [||]
        let mutable more = true

        while more do
            let b : byte = (byte)(vall &&& SEVEN_BIT_MASK_U)
            vall <- vall >>> 7
            more <- vall <> 0u
            let newVall =
                if more then b ||| CONTINUATION_BIT
                else b
            r <- Array.concat [ r; [| newVall |] ]

        r

    let i32 (v: int32) : byte array =
        let mutable vall = v
        let mutable r : byte array = [||]
        let mutable more = true
        let signBit = 64uy
        while more do
            let b : byte = (byte)(vall &&& SEVEN_BIT_MASK_S)
            let signBitSet = (b &&& signBit) <> 0uy
            
            vall <- vall >>> 7

            let nextVall = 
                if ((vall = 0 && (not signBitSet)) || (vall = -1 && signBitSet)) then
                    more <- false                
                    b
                else
                    b ||| CONTINUATION_BIT
            r <- Array.concat [ r; [| nextVall |] ]
        r

    let locals (n: int32) (b: byte) = Array.concat [ i32 n; [| b |] ]

    let section (id: byte) (contents: byte array) =
        let normalizedSize = i32 contents.Length
        let headers = Array.concat [ [| id |]; normalizedSize ]
        Array.concat [ headers; contents ]

    let vec (elements: byte array) =
        let normalizedSize = i32 elements.Length

        Array.concat [ normalizedSize
                       elements ]

    let vecFlatten (elements: byte array array) =
        let nonEmptyElements =
            elements
            |> Array.filter (fun ele -> ele.Length > 0)

        let normalizedSize = i32 nonEmptyElements.Length
        let flattenedElements = elements |> Array.collect id

        Array.concat [ normalizedSize
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
    let exportdesc (idx: byte array) = Array.concat [ [| 0uy |]; idx ]
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

        Array.concat [ normalizedSize
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
        | "%" -> INSTR_i32_MOD_S
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

    let rec private expressionToWasm
        (expr: Ast.Expression)
        (symbols: NestedSymbolDict)
        (symbolMap: SymbolDict)
        : byte array =
        match expr.ExprType() with
        | Ast.ExpressionType.IntegerLiteral ->
            let integerLiteral = expr :?> Ast.IntegerLiteral
            let value = i32 integerLiteral.value
            let wasmBytes = Array.concat [ [| INSTR_i32_CONST |] ; value ]
            wasmBytes
        | Ast.ExpressionType.InfixExpression ->
            let infixExpression = expr :?> Ast.InfixExpression
            let leftWasm = expressionToWasm infixExpression.left symbols symbolMap
            let operatorWasm = [| getOperator infixExpression.operator |]
            let rightWasm = expressionToWasm infixExpression.right symbols symbolMap

            let wasmBytes =
                Array.concat [ leftWasm
                               rightWasm
                               operatorWasm ]

            wasmBytes
        | Ast.ExpressionType.Identifier ->
            let iden = expr :?> Ast.Identifier

            let symbol = resolveSymbols symbolMap iden.value

            match symbol with
            | Ok sy ->
                let wasmBytes = Array.concat [ [| INSTR_LOCAL_GET |] ; i32 sy.index ]
                wasmBytes
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

            let mutable arguBytes: byte array = [||]

            for args in callExpr.arguments do
                let argTree = expressionToWasm args symbols symbolMap

                arguBytes <- Array.concat [ arguBytes; argTree ]

            let valueBytes = Array.concat [ [| INSTR_CALL |]; i32 index ]
            let wasmBytes = Array.concat [ arguBytes; valueBytes ]
            wasmBytes
        | Ast.ExpressionType.IfElseExpression ->
            let ifElseExpr = expr :?> Ast.IfElseExpression

            let exprWasm = expressionToWasm ifElseExpr.condition symbols symbolMap
            let ifCommandWasm = [| INSTR_IF; getBlockType I32 |]
            let thenBlockWasm = statementToWasm ifElseExpr.consequence symbols symbolMap
            let elseCommandWasm = [| INSTR_ELSE |]

            let elseBlockWasm = statementToWasm ifElseExpr.alternative.Value symbols symbolMap

            let endingWasm = [| INSTR_END |]

            let wasmBytes =
                Array.concat [ exprWasm
                               ifCommandWasm
                               thenBlockWasm
                               elseCommandWasm
                               elseBlockWasm
                               endingWasm ]

            wasmBytes
        | Ast.ExpressionType.AssignmentExpression ->
            let assignExpr = expr :?> Ast.AssignmentExpression
            let symbol = resolveSymbols symbolMap assignExpr.name.value

            match symbol with
            | Ok sy ->
                let exprBytes = expressionToWasm assignExpr.value symbols symbolMap

                let valueBytes =
                    Array.concat [
                    [| INSTR_LOCAL_TEE |];
                       i32 sy.index;
                       [| INSTR_DROP |] ]

                let wasmBytes = Array.concat [ exprBytes; valueBytes ]
                wasmBytes
            | Error msg -> raise (Exception(msg))        
        | _ -> [||]

    and private statementToWasm
        (state: Ast.Statement)
        (symbols: NestedSymbolDict)
        (symbolMap: SymbolDict)
        : byte array =
        match state.StateType() with
        | Ast.StatementType.LetStatement ->
            let letState = state :?> Ast.LetStatement
            let symbol = resolveSymbols symbolMap letState.name.value

            match symbol with
            | Ok sy ->
                let innerBytes = expressionToWasm letState.value symbols symbolMap
                let valueBytes = Array.concat [ [| INSTR_LOCAL_SET |]; i32 sy.index ]
                let wasmBytes = Array.concat [ innerBytes; valueBytes ]
                wasmBytes
            | Error msg -> raise (Exception(msg))
        | Ast.StatementType.ExpressionStatement ->
            let exprState = state :?> Ast.ExpressionStatement
            let wasmBytes = expressionToWasm exprState.expression symbols symbolMap
            wasmBytes
        | Ast.StatementType.BlockStatement ->
            let blockState = state :?> Ast.BlockStatement
            let mutable wasmBytes: byte array = [||]

            for state in blockState.statements do
                wasmBytes <-
                    Array.concat [ wasmBytes
                                   statementToWasm state symbols symbolMap ]

            wasmBytes
        | Ast.StatementType.FunctionStatement ->
            let fn = state :?> Ast.FunctionStatement

            let wasmBytes = statementToWasm fn.body symbols symbolMap
            wasmBytes
        | Ast.StatementType.WhileStatement ->
            let whileState = state :?> Ast.WhileStatement

            let loopBytes =
                [| INSTR_LOOP
                   getBlockType blockType.Empty_ |]

            let condBytes = expressionToWasm whileState.condition symbols symbolMap

            let ifBytes = [| INSTR_IF; getBlockType Empty_ |]

            let bodyBytes = statementToWasm whileState.body symbols symbolMap

            let brBytes =
                Array.concat [[| INSTR_BR |]; i32 1; [| INSTR_END; INSTR_END |] ]

            let wasmBytes =
                Array.concat [ loopBytes
                               condBytes
                               ifBytes
                               bodyBytes
                               brBytes ]

            wasmBytes
        | _ -> [||]

    let rec confirmFunctionDefined (expression: Ast.Expression) (scopes: SymbolScope) =
        match expression.ExprType() with
        | Ast.ExpressionType.CallExpression ->
            let callExpr = expression :?> Ast.CallExpression

            let funcName = callExpr.funcName
            let funcMapping = scopes.First.Value

            match funcMapping with
            | Nested (inner) ->
                if inner.ContainsKey funcName then
                    ()
                else
                    raise (new Exception($"The '{funcName}' function has not been defined before being called"))
            | _ -> ()
        | _ -> ()

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
                { name = name
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
        | Ast.StatementType.ExpressionStatement ->
            let expr = statement :?> Ast.ExpressionStatement

            confirmFunctionDefined expr.expression scopes

            ()
        | Ast.StatementType.WhileStatement ->
            let whileState = statement :?> Ast.WhileStatement

            buildSymbolTable whileState.body scopes
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

            let bodyWasm = statementToWasm funcState symbols funcSymbols

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
            |> Array.mapi (fun i x -> i32 i )
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

    let confirmMainFunction (symbolScope: SymbolScope) =
        let mainName = "main"
        match symbolScope.First.Value with
        | Nested inner ->
            if inner.ContainsKey mainName
            then
                let (symbolDict, _) = inner[mainName]
                let paramCount = 
                    symbolDict.Values
                    |> Seq.where (fun st -> st.symbolType = SymbolType.Param)
                    |> Seq.length
                if paramCount = 0
                then
                    ()
                else
                    raise (new Exception("Waux requires a zero parameter 'main' function to exist"))
            else
                raise (new Exception("Waux requires a zero parameter 'main' function to exist"))
        | Locals locals -> raise (new Exception("Should not be locals"))
            

    let compile (codeModule: Ast.Module) =
        let symbolScope = buildSymbolMap codeModule
        confirmMainFunction symbolScope

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
