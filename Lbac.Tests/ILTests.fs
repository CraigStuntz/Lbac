namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open IL

[<TestClass>]
type ILTests() = 
    [<TestMethod>]
    member x.``should produce an executable assembly`` () = 
        let input = [Ldc_I4(1); Ldc_I4(2); instruction.Add] // 1 + 2
        let expected = 1 + 2
        let mi = IL.toMethod(input, typedefof<System.Int32>)
        let instance = Activator.CreateInstance(mi.DeclaringType)
        let actual = mi.Invoke(instance, null) :?> System.Int32
        Assert.AreEqual(expected, actual)
    