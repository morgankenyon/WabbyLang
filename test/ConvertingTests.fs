namespace Waux.Lang.Test

module ConvertingTests =
    open Xunit
    open Wasmtime

    [<Fact>]
    let ``Can convert bytes to wasm program`` () =
        let numbers =
            [| 0
               97
               115
               109
               1
               0
               0
               0
               1
               6
               1
               96
               1
               127
               1
               127
               3
               2
               1
               0
               7
               6
               1
               2
               102
               49
               0
               0
               10
               15
               1
               13
               1
               1
               127
               65
               42
               33
               1
               32
               0
               32
               1
               106
               11 |]

        let bytes = numbers |> Array.map (fun num -> (byte) num)
        Assert.NotNull bytes
    //Helpers.printWasm bytes
    //let engine = new Engine()

    //let modd = Module.FromBytes(engine, "testLang", bytes)

    //let watText = modd.ToString();
    //Assert.NotEmpty(watText)

    [<Fact>]
    let ``Can convert book bytes to wasm program`` () =
        //let x = 42; x
        let numbers =
            [| 0
               97
               115
               109
               1
               0
               0
               0
               1
               5
               1
               96
               0
               1
               127
               3
               2
               1
               0
               7
               8
               1
               4
               109
               97
               105
               110
               0
               0
               10
               12
               1
               10
               1
               1
               127
               65
               42
               33
               0
               32
               0
               11 |]

        let bytes = numbers |> Array.map (fun num -> (byte) num)
        Assert.NotNull bytes
    //Helpers.printWasm bytes
    //let engine = new Engine()

    //let modd = Module.FromBytes(engine, "testLang", bytes)

    //let watText = modd.ToString();
    //Assert.NotEmpty(watText)
    [<Fact>]
    let ``Can convert multi function example to wasm program`` () =
        let numbers =
            [| 0
               97
               115
               109
               1
               0
               0
               0
               1
               5
               1
               96
               0
               1
               127
               3
               3
               2
               0
               0
               7
               17
               2
               4
               109
               97
               105
               110
               0
               0
               6
               98
               97
               99
               107
               117
               112
               0
               1
               10
               13
               2
               6
               1
               1
               127
               65
               42
               11
               4
               0
               65
               43
               11 |]

        let bytes = numbers |> Array.map (fun num -> (byte) num)
        Assert.NotNull bytes
//Helpers.printWasm bytes
    [<Fact>]
    let ``Can convert while loop example to wasm program`` () =
        let numbers =
            [|    0;  97; 115; 109;   1;  0;   0;  0; 1;   6;  1; 96;
   1; 127;   1; 127;   3;  2;   1;  0; 7;  11;  1;  7;
  99; 111; 117; 110; 116; 84; 111;  0; 0;  10; 33;  1;
  31;   1;   1; 127;  65;  0;  33;  1; 3;  64; 32;  1;
  32;   0;  72;   4;  64; 32;   1; 65; 1; 106; 34;  1;
  26;  12;   1;  11;  11; 32;   1; 11 |]

        let bytes = numbers |> Array.map (fun num -> (byte) num)
        Assert.NotNull bytes
        Helpers.printWasm bytes
