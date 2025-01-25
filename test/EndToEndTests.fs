namespace Waux.Lang.Test

module EndToEndTests =
    open Xunit
    open Waux.Lang
    open Wasmtime

    [<Fact>]
    let ``Can compile empty number`` () =
        let input = "5"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile addition operation`` () =
        let input = "5 + 2"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile subtraction operation`` () =
        let input = "10 - 2"

        let result = 
            EndToEnd.compileInstantiateAndPrint input false
            |> Helpers.runWithInt32Return

        Assert.Equal(8, result)

    [<Fact>]
    let ``Can compile combined operation`` () =
        let input = "5 - 2 + 7"

        EndToEnd.compileInstantiateAndPrint input false

    [<Fact>]
    let ``Can compile multiplication operation`` () =
        let input = "5 * 7"

        let result = 
            EndToEnd.compileInstantiateAndPrint input false
            |> Helpers.runWithInt32Return

        Assert.Equal(result, 35)

    [<Fact>]
    let ``Can compile division operation`` () =
        let input = "10 / 5"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 2)

    [<Fact>]
    let ``Can compile and run combined operation`` () =
        let input = "5 - 2 + 7"

        let bytes = EndToEnd.compile input

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 10)

    [<Fact>]
    let ``Can compile all operation formula`` () =
        let input = "10 + 10 / 5 * 2 - 1"

        let bytes = EndToEnd.compileInstantiateAndPrint input false

        let result = Helpers.runWithInt32Return bytes

        Assert.Equal(result, 13)



