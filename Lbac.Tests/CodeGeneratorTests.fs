namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open Errors
open IL
open Syntax

[<TestClass>]
type CodeGeneratorTests() = 
    [<TestMethod>]
    member x.``should codegen 1 + 2 * 0`` () = 
        let testVal = Expr.Binary(Expr.Binary(Expr.Number(1), Add, Expr.Number(2)), Multiply, Expr.Number(0))
        let expected = { Instructions = [Ldc_I4(1); Ldc_I4(2); instruction.Add; Ldc_I4_0; Mul]; Locals = [] }
        let actual = CodeGenerator.codegen(Success(testVal))
        match actual with 
            | Success il -> Assert.AreEqual(expected, il)
            | Error e -> Assert.Fail(e)


