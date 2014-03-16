namespace Lbac.Tests 

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open CodeGenerator
open Railway
open IL
open Syntax

[<TestClass>]
type CodeGeneratorTests() =
    let shouldFailWith (input, locals) error = 
        match CodeGenerator.codegen({ Lines = [input]; Locals = locals }) with
            | Success _ -> Assert.Fail("Expected error.")
            | Failure e -> Assert.AreEqual(error, e)

    let shouldProduceIL (input, locals) expected = 
        let actual = CodeGenerator.codegen({ Lines = [input]; Locals = locals })
        match actual with 
            | Success il -> Assert.AreEqual(expected, il.Instructions)
            | Failure e -> Assert.Fail(e)

    let noLocalVariables = Set.empty

    [<TestMethod>]
    member x.``should codegen 1 + 2 * 0`` () = 
        let input = Expr.Binary(Expr.Binary(Expr.Number(1), Add, Expr.Number(2)), Multiply, Expr.Number(0))
        let expected = [Ldc_I4 1; Ldc_I4 2; instruction.Add; Ldc_I4 0; Mul]
        (input, noLocalVariables) |> shouldProduceIL <| expected

    [<TestMethod>]
    member x.``should fail on undeclared variable`` () =
        let input = Expr.Binary(Expr.Variable("x"), Add, Expr.Number(1))
        let expected = "Undeclared variable x"
        (input, noLocalVariables) |> shouldFailWith <| expected
    
    [<TestMethod>]
    member x.``should codegen x + 1`` () =
        let input = Expr.Binary(Expr.Variable("x"), Add, Expr.Number(1))
        let expected = [DeclareLocal(typedefof<int>); Ldloc_S 0uy; Ldc_I4 1; instruction.Add]
        (input, Set.singleton("x")) |> shouldProduceIL <| expected
    
    [<TestMethod>]
    member x.``should codegen x = 1`` () =
        let input = Expr.Assign(Expr.Variable("x"), Expr.Number(1))
        let expected = [DeclareLocal(typedefof<int>); Ldc_I4(1); Stloc_S 0uy]
        (input, Set.singleton("x")) |> shouldProduceIL <| expected