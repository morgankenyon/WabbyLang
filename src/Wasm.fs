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
    let INSTR_END = 11uy

    type WasmValueBytes =
    | Value of byte
    | Values of byte array

    type WasmTree =
    | Empty
    | Node of value: WasmValueBytes array option * inners: WasmTree array option
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
        let mutable wasmTree : WasmTree option = None
        let aType = expr.AType()
        match aType with
        | Ast.AstType.IntegerLiteral ->
            let integerLiteral = expr :?> Ast.IntegerLiteral
            let value = i32 integerLiteral.value
            let wasmBytes = [| INSTR_i32_CONST; value |]
            let node = Node (Some [| Values wasmBytes |], None )
            node
        | Ast.AstType.InfixExpression ->
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
            //wasmTree <- None

        //wasmTree
    let private generateBytes (bytes: byte array) (v: WasmValueBytes) =
        match v with
        | Value vv ->
            Array.concat [ bytes; [| vv |] ]
        | Values vvs ->
            Array.concat [ bytes; vvs ]

    let rec private wasmTreeToBytes (tree : WasmTree) : byte array =
        match tree with
        | Empty -> [| |]
        | Node (value, inners) when value.IsSome && inners.IsNone ->
            let vals = value.Value
            let mutable bytes: byte array = [| |]
            for v in vals do
                bytes <- generateBytes bytes v
            bytes
        | Node (value, inners) when value.IsNone && inners.IsSome ->
            let vals = inners.Value
            let mutable bytes: byte array = [| |]
            for v in vals do
                let innerBytes = wasmTreeToBytes v
                bytes <- Array.concat [ bytes; innerBytes ]
            bytes
        | _ -> [| |]


    let generateWasm (codeModule : Ast.Module) : byte array =
        let firstExpr = codeModule.expressions.[0]
        let wasmTree = expressionToWasmTree firstExpr
        let bytes = wasmTreeToBytes wasmTree
        ///Array.concat [ bytes; [| INSTR_END |] ]
        Array.concat [ bytes; [| INSTR_END |] ]

        //let mutable wasmBytes : byte array option = None
        //for expr in codeModule.expressions do
        //    //let aType = expr.AType()
        //    let aType = expr.AType()
        //    match aType with
        //    | Ast.AstType.IntegerLiteral ->
        //        let integerLiteral = expr :?> Ast.IntegerLiteral
        //        let value = i32 integerLiteral.value
        //        wasmBytes <- Some [| INSTR_i32_CONST; value |]
        //    //| Ast.AstType.InfixExpression ->
        //    //    let infixExpression = expr :?> Ast.InfixExpression
        //    //    let leftValue = i32 infixExpression.left
        //    //    let operator = infixExpression.operator
        //    //    let rightValue = i32 infixExpression.right
        //    | _ ->
        //        wasmBytes <- None
        //match wasmBytes with
        //| Some bytes ->
        //    Array.concat [ bytes; [| INSTR_END |] ]
        //| None -> 
        //    let emptyBytes: byte [] = Array.zeroCreate 0
        //    emptyBytes
        

    let toWasm (codeModule : Ast.Module) =
        let emptyBytes: byte [] = Array.zeroCreate 0
        //let magic = magic()
        //let version = version()

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
        //let instrEnd = 11uy
        let functions = 
            generateWasm codeModule
            |> func emptyBytes
        //let functions = func emptyBytes emptyBytes
        let code = code functions
        let codesec = codesec [| code |]

        let bytes = modd [| typesec; funcsec; exportsec; codesec |]
        bytes
        