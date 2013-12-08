namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Compiler
open Errors

[<TestClass>]
type EndToEndTests() = 
    let execute (mi: System.Reflection.MethodInfo) =
        let instance = Activator.CreateInstance(mi.DeclaringType)
        mi.Invoke(instance, null) :?> System.Int32

    let shouldEqual output expected = 
        match output with
        | Success mi -> Assert.AreEqual(expected, execute(mi))
        | Error e -> Assert.Fail(e)
         
    [<TestMethod>]
    member x.``1 + 2 should equal 3`` () = 
        Compiler.compile("1 + 2") |> shouldEqual <| 3

    [<TestMethod>]
    member x.``1 - 2 should equal -1`` () = 
        Compiler.compile("1 - 2") |> shouldEqual <| -1

    [<TestMethod>]
    member x.``2 * 3 should equal 6`` () = 
        Compiler.compile("2 * 3") |> shouldEqual <| 6

    [<TestMethod>]
    member x.``10 - 2 * 3 should equal 4`` () = 
        Compiler.compile("10 - 2 * 3") |> shouldEqual <| 4

    [<TestMethod>]
    member x.``(10 - 2) * 3 should equal 24`` () = 
        Compiler.compile("(10 - 2) * 3") |> shouldEqual <| 24

    [<TestMethod>]
    member x.``-2 + 2 should equal 0`` () = 
        Compiler.compile("-2 + 2") |> shouldEqual <| 0

    [<TestMethod>]
    member x.``2+-3 should equal -1`` () = 
        Compiler.compile("2+-3") |> shouldEqual <| -1

    [<TestMethod>]
    member x.``6/2 should equal 3`` () = 
        Compiler.compile("6/2") |> shouldEqual <| 3

    [<TestMethod>]
    member x.``Assign and use local var`` () = 
        Compiler.compile("x = 1\nx + 2") |> shouldEqual <| 3

    [<TestMethod>]
    member x.``Assign and use 2 local vars`` () = 
        Compiler.compile("x = 1\ny = 2\nx + y") |> shouldEqual <| 3