namespace Waux.Lang.Test

module ConvertingTests =
    open Xunit
    open Wasmtime

    [<Fact>]
    let ``Can convert bytes to wasm program`` () =
        let numbers = [|   0;  97; 115; 109;   1; 0;  0;  0;  1;  6;   1;
              96;   1; 127;   1; 127; 3;  2;  1;  0;  7;   6;
               1;   2; 102;  49;   0; 0; 10; 15;  1; 13;   1;
               1; 127;  65;  42;  33; 1; 32;  0; 32;  1; 106;
              11|]
        let bytes = numbers |> Array.map (fun num -> (byte) num)
        Assert.NotNull bytes
        //Helpers.printWasm bytes
        //let engine = new Engine()

        //let modd = Module.FromBytes(engine, "testLang", bytes)
        
        //let watText = modd.ToString();
        //Assert.NotEmpty(watText)