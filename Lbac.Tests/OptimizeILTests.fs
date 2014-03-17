namespace Lbac.Tests

open IL
open OptimizeIL
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type OptimizeILTests() =

    [<TestMethod>]
    member x.``should change Ldc_I4 2 to LDC_I4_2`` () = 
        let input = { Instructions = [Ldc_I4 2]; Locals = [] }
        let actual = optimize input
        Assert.AreEqual([Ldc_I4_2], actual.Instructions)
