namespace Waux.Lang.Test

module EndToEndTests =
    open Xunit
    open Waux.Lang

    [<Fact>]
    let ``Can compile empty number`` () =
        let input = "5"

        EndToEnd.compileInstantiateAndPrint input false

