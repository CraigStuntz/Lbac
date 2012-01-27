namespace Lbac.Tests

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System.IO
    open System.Text
    open Lbac

    [<TestClass>]
    type CradleTests() = class
        [<TestMethod>]
        member x.testInit() = 
            let actual = new StringBuilder()

            let cradle = new Cradle(new StringReader("1"))
            cradle.compile() |> ignore
    end




