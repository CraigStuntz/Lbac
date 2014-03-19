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

    [<TestMethod>]
    member x.``should leave Stloc 100000 as Stloc 100000`` () = 
        let input = { Instructions = [Ldc_I4_1; Stloc 100000; Ldloc 100000]; Locals = [] }
        let actual = optimize input
        Assert.AreEqual([Ldc_I4_1; Stloc 100000; Ldloc 100000], actual.Instructions)

    [<TestMethod>]
    member x.``should change Stloc 10 to Stloc_S 10`` () = 
        let input = { Instructions = [Ldc_I4_1; Stloc 10; Ldloc 10]; Locals = [] }
        let actual = optimize input
        Assert.AreEqual([Ldc_I4_1; Stloc_S 10uy; Ldloc_S 10uy], actual.Instructions)

    [<TestMethod>]
    member x.``should change Stloc 1 to Stloc_1`` () = 
        let input = { Instructions = [Ldc_I4_1; Stloc 1; Ldloc 1]; Locals = [] }
        let actual = optimize input
        Assert.AreEqual([Ldc_I4_1; Stloc_1; Ldloc_1], actual.Instructions)
