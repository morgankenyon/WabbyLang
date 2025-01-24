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
//    const valtype = {
//  i32: 0x7f,
//  i64: 0x7e,
//  f32: 0x7d,
//  f64: 0x7c,
//};

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

    //let u32 (v: int) =
    //    if v <= 127 then byte v else 0uy

    let u32 (v: uint) =
        if v <= 127u then byte v else 0uy
    
    let i32 (v: int) =
        if v <= 63 then byte v else 0uy

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

    //Code section
        

    let func (locals: byte array) (body: byte array) =
        let localsVec = vec locals
        Array.concat [ localsVec; body ]

    let funcCombined (wasmBytes: WasmFuncBytes) =
        let localsVec = vec wasmBytes.locals
        Array.concat [ localsVec; wasmBytes.body ]

    let code (func: byte array) =
        let normalizedSize = i32 func.Length
        Array.concat [ [| normalizedSize |]; func ]

    let codesec (codes: byte array array) =
        vecFlatten codes
        |> section SECTION_ID_CODE

    //let codeSecTree (locals: WasmTree) (body: WasmTree) =
    //    match locals, body with
    //    | Node (lv, li), Empty ->
    //        //vec - size
    //        let localVec = i32 i.Value.Length
    //        let funcSize = localVec
    //        //let normalizedSize = i32 n.
    //    | Empty ->


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
        | _ -> 11uy

    let rec private expressionToWasmTree (expr : Ast.Expression) : WasmTree =
        match expr.ExprType() with
        | Ast.ExpressionType.IntegerLiteral ->
            let integerLiteral = expr :?> Ast.IntegerLiteral
            let value = i32 integerLiteral.value
            let wasmBytes = [| INSTR_i32_CONST; value |]
            let node = Node (Some [| Values wasmBytes |], None )
            node
        | Ast.ExpressionType.InfixExpression ->
            let infixExpression = expr :?> Ast.InfixExpression
            let leftValue = expressionToWasmTree infixExpression.left
            let operator = getOperator infixExpression.operator
            let operatorValue = Node (Some [| Value operator |], None)
            let rightValue = expressionToWasmTree infixExpression.right
            let inner = [| leftValue; rightValue; operatorValue |]
            let node = Node (None, Some inner)
            node
        | _ -> 
            Empty

    let rec private statementToWasmTree (state: Ast.Statement) : WasmTree =
        match state.StateType() with
        | Ast.StatementType.LetStatement ->
            Empty
        | Ast.StatementType.ExpressionStatement -> 
            let exprState = state :?> Ast.ExpressionStatement
            expressionToWasmTree exprState.expression
        | _ -> Empty

    let private generateBytes (bytes: byte array) (v: WasmValueBytes) =
        match v with
        | Value vv ->
            Array.concat [ bytes; [| vv |] ]
        | Values vvs ->
            Array.concat [ bytes; vvs ]

    let rec private wasmTreeToBytes (tree : WasmTree) : WasmFuncBytes =
        match tree with
        | Empty -> 
            { body = [| |]; locals = [| |] }
        | Node (value, inners) when value.IsSome && inners.IsNone ->
            let vals = value.Value
            let mutable bytes: byte array = [| |]
            for v in vals do
                bytes <- generateBytes bytes v
            { body = bytes; locals = [| |] }
        | Node (value, inners) when value.IsNone && inners.IsSome ->
            let vals = inners.Value
            let mutable bytes: byte array = [| |]
            for v in vals do
                let innerBytes = wasmTreeToBytes v
                bytes <- Array.concat [ bytes; innerBytes.body ]
            { body = bytes; locals = [| |] }
        | _ -> { body = [| |]; locals = [| |] }


    let generateWasm (codeModule : Ast.Module) : WasmFuncBytes =
        let firstStatement = codeModule.statements.[0]
        let wasmTree = statementToWasmTree firstStatement
        let wasmBytes = wasmTreeToBytes wasmTree

        let bodyBytes = Array.concat [ wasmBytes.body; [| INSTR_END |]]

        { body = bodyBytes; locals = wasmBytes.locals }
     
    let rec private convertToSymbolMap (symbolMap: System.Collections.Generic.Dictionary<string, SymbolEntry>) (statement: Ast.Statement) =
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
        let symbolMap = new System.Collections.Generic.Dictionary<string, SymbolEntry>();

        convertToSymbolMap symbolMap codeModule
        symbolMap

    let toWasm (codeModule : Ast.Module) =
        let emptyBytes: byte [] = Array.zeroCreate 0

        //Creating type section
        let funcType = functype(emptyBytes, [| i32_VAL_TYPE |])
        let typesec = typesec([| funcType |])
            
        //creating func section
        let funcsec = funcsec([| [|0uy|] |])

        //creating export section
        let exportDesc = exportdesc(0uy)
        let export = export "main" exportDesc
        let exportsec = exportsec [| export |]

        //creating code section
        let functions = 
            generateWasm codeModule
            |> funcCombined
        //let functions = func emptyBytes emptyBytes
        let code = code functions
        let codesec = codesec [| code |]

        let bytes = modd [| typesec; funcsec; exportsec; codesec |]
        bytes
        