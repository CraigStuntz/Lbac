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
