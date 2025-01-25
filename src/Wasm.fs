namespace Waux.Lang

module Wasm =
    open System

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
    let INSTR_DROP = 26uy

    type WasmValueBytes =
    | Value of byte
    | Values of byte array

    type WasmTree =
    | Empty
    | Node of value: WasmValueBytes array option * inners: WasmTree array option

    type WasmFuncBytes =
        {
            locals: byte array
            body: byte array
        }
    type SymbolType =
    | Local
    type SymbolEntry =
        {
            name: string
            index: int
            symbolType: SymbolType 
        }
    type SymbolMapDict = System.Collections.Generic.Dictionary<string, SymbolEntry>

    let stringToBytes (s: string) =
        System.Text.Encoding.UTF8.GetBytes(s)

    let int32ToBytes (v: int32) =
        let bytes = BitConverter.GetBytes(v);
        match BitConverter.IsLittleEndian with
        | true -> bytes
        | false -> Array.rev bytes

    let magic () =
        // [0x00, 0x61, 0x73, 0x6d]
        let nullChar = Convert.ToChar(0).ToString()
        stringToBytes($"{nullChar}asm")

    let version () =
        // [0x01, 0x00, 0x00, 0x00]
        int32ToBytes(1)

    let u32 (v: uint) =
        if v <= 127u then byte v else 0uy
    
    let i32 (v: int) =
        if v <= 63 then byte v else 0uy

    let locals(n: int32) (b: byte) =
        [| i32 n; b |]

    let section (id: byte) (contents: byte array) =
        let normalizedSize = i32 contents.Length
        let headers = [| id ; normalizedSize |]
        Array.concat [ headers; contents ]

    let vec (elements: byte array) =
        let normalizedSize = i32 elements.Length
        Array.concat [ [|normalizedSize |]; elements]

    let vecFlatten (elements: byte array array) =
        let normalizedSize = i32 elements.Length
        let flattenedElements = elements
                                |> Array.collect id
        Array.concat [ [| normalizedSize |]; flattenedElements ]

    //Type Section
    let functype (paramTypes: byte array, resultTypes: byte array) =
        let paramVec = vec paramTypes
        let resultVec = vec resultTypes
        Array.concat [ [| TYPE_FUNCTION |]; paramVec; resultVec ]

    let typesec (functypes : byte array array) =
        let funcVec = vecFlatten functypes
        section SECTION_ID_TYPE funcVec

    //Function Section
    let funcsec (typeidxs : byte array array) =
        vecFlatten typeidxs
        |> section SECTION_ID_FUNCTION

    //Export section
    let exportdesc (idx: byte) =
        [| 0uy; idx |]
    let name (s: string) =
        s
        |> stringToBytes
        |> vec

    let export (s: string) (exportDesc: byte array) =
        Array.concat [ name(s) ; exportDesc ]

    let exportsec (exports: byte array array) =
        vecFlatten exports
        |> section SECTION_ID_EXPORT

    //Code Section
    let func (locals: byte array) (body: byte array) =
        let localsVec = vec locals
        Array.concat [ localsVec; body ]
    let funcNested (locals: byte array array) (body: byte array) =
        let localsVec = vecFlatten locals
        Array.concat [ localsVec; body ]

    let code (func: byte array) =
        let normalizedSize = i32 func.Length
        Array.concat [ [| normalizedSize |]; func ]

    let codesec (codes: byte array array) =
        vecFlatten codes
        |> section SECTION_ID_CODE

    let modd(sections: byte array array) =
        let flattenedSections = sections
                                |> Array.collect id
        Array.concat [ magic(); version(); flattenedSections ]
    
    let getOperator op =
        match op with
        | "+" -> INSTR_i32_ADD
        | "-" -> INSTR_i32_SUB
        | "*" -> INSTR_i32_MUL
        | "/" -> INSTR_i32_DIV_S
        | _ -> INSTR_END

    let resolveSymbols (symbolMap: SymbolMapDict) (name: string) =
        let containsKey = symbolMap.ContainsKey name
        match containsKey with
        | true ->            
            let symbol = symbolMap[name]
            Ok symbol
        | false ->
            Error $"Error: undeclared identifier: {name}"

    let rec private expressionToWasmTree (expr : Ast.Expression) (symbolMap: SymbolMapDict): WasmTree =
        match expr.ExprType() with
        | Ast.ExpressionType.IntegerLiteral ->
            let integerLiteral = expr :?> Ast.IntegerLiteral
            let value = i32 integerLiteral.value
            let wasmBytes = [| INSTR_i32_CONST; value |]
            let node = Node (Some [| Values wasmBytes |], None )
            node
        | Ast.ExpressionType.InfixExpression ->
            let infixExpression = expr :?> Ast.InfixExpression
            let leftValue = expressionToWasmTree infixExpression.left symbolMap
            let operator = getOperator infixExpression.operator
            let operatorValue = Node (Some [| Value operator |], None)
            let rightValue = expressionToWasmTree infixExpression.right symbolMap
            let inner = [| leftValue; rightValue; operatorValue |]
            let node = Node (None, Some inner)
            node
        | Ast.ExpressionType.Identifier -> 
            let iden = expr :?> Ast.Identifier
            
            let symbol = resolveSymbols symbolMap iden.value
            match symbol with
            | Ok sy ->
                let wasmBytes = [| INSTR_LOCAL_GET; i32 sy.index |]
                let node = Node (Some [| Values wasmBytes |], None)
                node
            | Error msg -> raise (Exception(msg))
        | _ ->
            Empty

    let rec private statementToWasmTree (state: Ast.Statement) (symbolMap: SymbolMapDict) : WasmTree =
        match state.StateType() with
        | Ast.StatementType.LetStatement ->
            let letState = state :?> Ast.LetStatement
            let symbol = resolveSymbols symbolMap letState.name.value
            match symbol with
            | Ok sy ->
                let expreTree = expressionToWasmTree letState.value symbolMap
                let wasmBytes = [| INSTR_LOCAL_SET; i32 sy.index |]
                let node = Node (Some [| Values wasmBytes |], Some [| expreTree |])
                node
            | Error msg -> raise (Exception(msg))
        | Ast.StatementType.ExpressionStatement -> 
            let exprState = state :?> Ast.ExpressionStatement
            let exprTree = expressionToWasmTree exprState.expression symbolMap
            //let wasmBytes = [| INSTR_DROP |]
            let node = Node (None, Some [| exprTree |])
            node
        | _ -> Empty

    let private generateBytes (bytes: byte array) (v: WasmValueBytes) =
        match v with
        | Value vv ->
            Array.concat [ bytes; [| vv |] ]
        | Values vvs ->
            Array.concat [ bytes; vvs ]

    let rec private wasmTreeToBytes (tree : WasmTree) : byte array =
        match tree with
        | Empty -> 
            [| |]
        | Node (value, inners) when value.IsSome && inners.IsNone ->
            let vals = value.Value
            let mutable bytes: byte array = [| |]
            for v in vals do
                bytes <- generateBytes bytes v
            bytes
        | Node (value, inners) when value.IsNone && inners.IsSome ->
            let inn = inners.Value
            let mutable bytes: byte array = [| |]
            for v in inn do
                let innerBytes = wasmTreeToBytes v
                bytes <- Array.concat [ bytes; innerBytes ]
            bytes
        | Node (value, inners) when value.IsSome && inners.IsSome ->
            let vals = value.Value
            let inn = inners.Value
            let mutable bytes: byte array = [| |]
            for i in inn do
                let innerBytes = wasmTreeToBytes i
                bytes <- Array.concat [ bytes; innerBytes ]
            for v in vals do
                bytes <- generateBytes bytes v
            bytes
        | _ -> [| |]


    let generateWasm (codeModule : Ast.Module) (symbolMap : SymbolMapDict) : byte array =
        let mutable bytes : byte array = [| |]
        for statement in codeModule.statements do
            let wasmTree = statementToWasmTree statement symbolMap
            let wasmBytes = wasmTreeToBytes wasmTree
            bytes <- Array.concat [ bytes; wasmBytes ]

        let bodyBytes = Array.concat [ bytes; [| INSTR_END |]]
        bodyBytes
     
    let rec private convertToSymbolMap (symbolMap: SymbolMapDict) (statement: Ast.Statement) =
        match statement.StateType() with
        | Ast.StatementType.Module ->
            let modd = statement :?> Ast.Module
            for state in modd.statements do
                convertToSymbolMap symbolMap state
            ()
        | Ast.StatementType.LetStatement ->
            let letState = statement :?> Ast.LetStatement
            let name = letState.name.value
            let symbolEntry = 
                {
                    name = name
                    index = symbolMap.Count
                    symbolType = SymbolType.Local
                }
            symbolMap.Add(name, symbolEntry)
        | _ -> ()

    let buildSymbolMap (codeModule : Ast.Module) =
        let symbolMap = new SymbolMapDict();

        convertToSymbolMap symbolMap codeModule
        symbolMap


    let toWasm (codeModule : Ast.Module) =
        let emptyBytes: byte [] = Array.zeroCreate 0
        let symbolMap = buildSymbolMap codeModule

        //Creating type section
        //need to figure out programmatically
        let funcType = functype(emptyBytes, [| i32_VAL_TYPE |])
        let typesec = typesec([| funcType |])
            
        //creating func section
        let funcsec = funcsec([| [|0uy|] |])

        //creating export section
        let exportDesc = exportdesc(0uy)
        let export = export "main" exportDesc
        let exportsec = exportsec [| export |]

        //creating code section
        let localBytes = locals symbolMap.Count i32_VAL_TYPE
        let functions = 
            generateWasm codeModule symbolMap
            |> funcNested [| localBytes |]

        let code = code functions
        let codesec = codesec [| code |]

        let bytes = modd [| typesec; funcsec; exportsec; codesec |]
        bytes

    let buildModuleClean (codeModule: Ast.Module) =
        let emptyBytes: byte [] = Array.zeroCreate 0
        let symbolMap = buildSymbolMap codeModule

        //Creating type section
        //need to figure out programmatically
        let typesec = 
            functype(emptyBytes, [| i32_VAL_TYPE |])
            |> fun ft -> [| ft |]
            |> typesec
            
        //creating func section
        let funcsec =
            [| 0uy |]
            |> fun fs -> [| fs |]
            |> funcsec

        //creating export section
        let exportsec = 
            exportdesc(0uy)
            |> export "main"
            |> fun ed -> [| ed |]
            |> exportsec

        //creating code section
        let codesec = 
            generateWasm codeModule symbolMap
            |> funcNested [| locals symbolMap.Count i32_VAL_TYPE |]
            |> code
            |> fun c -> [| c |]
            |> codesec

        let bytes = modd [| typesec; funcsec; exportsec; codesec |]
        bytes

    let buildModule () =
        let emptyBytes: byte [] = Array.zeroCreate 0
        //let symbolMap = buildSymbolMap codeModule

        //creating code section
        let mainFn = 
            [| INSTR_i32_CONST; i32 42; INSTR_END |]
            |> funcNested [| locals 1 i32_VAL_TYPE |]
        let codesec = 
            mainFn
            |> code
            |> fun c -> [| c |]
            |> codesec

        //Creating type section
        //need to figure out programmatically
        let typesec = 
            functype(emptyBytes, [| i32_VAL_TYPE |])
            |> fun ft -> [| ft |]
            |> typesec
            
        //creating func section
        let funcsec =
            [| 0uy |]
            |> fun fs -> [| fs |]
            |> funcsec

        //creating export section
        let exportsec = 
            exportdesc(0uy)
            |> export "main"
            |> fun ed -> [| ed |]
            |> exportsec

        let bytes = modd [| typesec; funcsec; exportsec; codesec |]
        bytes
        