namespace Waux.Lang.Test

module Helpers =
    open Wasmtime
    let runWithInt32Return (wasmBytes : byte array) =
                
        let engine = new Engine()

        let modd = Module.FromBytes(engine, "wauxLang", wasmBytes)

        let linker = new Linker(engine)
        let store = new Store(engine)

        let instance = linker.Instantiate(store, modd)

        let main = instance.GetFunction<int32>("main")
        main.Invoke()
    let printWasm (bytes: byte array) =
        let stringRepresentation = bytes
                                    |> Array.map (fun by -> by.ToString())
                                    |> String.concat ""
        //let str = stringRepresentation |> String.concat ""
        System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
        System.IO.File.WriteAllBytes("./atest.wasm", bytes)

