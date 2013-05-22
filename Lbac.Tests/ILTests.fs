namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open Errors
open IL

[<TestClass>]
type ILTests() = 
    [<TestMethod>]
    member x.``should produce an executable assembly`` () = 
        let input = { Instructions = [Ldc_I4(1); Ldc_I4(2); instruction.Add]; Locals = [] } // 1 + 2
        let expected = 1 + 2
        let mi = IL.toMethod(Success(input), typedefof<System.Int32>)
        match mi with
            | Success methodInfo -> 
                let instance = Activator.CreateInstance(methodInfo.DeclaringType)
                let actual = methodInfo.Invoke(instance, null) :?> System.Int32
                Assert.AreEqual(expected, actual)
            | Error e -> Assert.Fail(e)
    