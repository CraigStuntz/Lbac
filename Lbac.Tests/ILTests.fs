namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open Railway
open IL

[<TestClass>]
type ILTests() = 
    [<TestMethod>]
    member x.``should produce an executable assembly`` () = 
        let input = { Instructions = [Ldc_I4(1); Ldc_I4(2); instruction.Add]; Locals = [] } // 1 + 2
        let expected = 1 + 2
        let methodInfo = IL.toMethod typedefof<System.Int32> input
        let instance = Activator.CreateInstance(methodInfo.DeclaringType)
        let actual = methodInfo.Invoke(instance, null) :?> System.Int32
        Assert.AreEqual(expected, actual)
    