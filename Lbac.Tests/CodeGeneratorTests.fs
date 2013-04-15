namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open IL
open Syntax

[<TestClass>]
type CodeGeneratorTests() = 
    [<TestMethod>]
    member x.``should codegen 1 + 2 * 0`` () = 
        let testVal = Expr.Binary(Expr.Binary(Expr.Number(1), Add, Expr.Number(2)), Multiply, Expr.Number(0))
        let expected = [Ldc_I4(1); Ldc_I4(2); instruction.Add; Ldc_I4_0; Mul]
        let actual = CodeGenerator.codegen(testVal)
        Assert.AreEqual(expected, actual)


