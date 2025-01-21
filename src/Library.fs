namespace Wabby.Lang

module Parser =
    open System

    [<Literal>]
    let SECTION_ID_TYPE = 1uy
    [<Literal>]
    let TYPE_FUNCTION = 96uy

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

    let section (id: uint, contents: byte array) =
        let normalizedSize = i32 contents.Length
        let headers = [|byte id; normalizedSize |]
        Array.concat [ headers; contents]

    let vec (elements: byte array) =
        let normalizedSize = [| i32 elements.Length |]
        Array.concat [ normalizedSize; elements ]

    let functype (paramTypes: byte array, resultTypes: byte array) =
        let paramVec = vec paramTypes
        let resultVec = vec resultTypes
        Array.concat [ [| TYPE_FUNCTION |]; paramVec; resultVec ]

    let typesec (functypes : byte array) =
        let funcVec = vec functypes
        Array.concat [ [| SECTION_ID_TYPE |]; funcVec ]

