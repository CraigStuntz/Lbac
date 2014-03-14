namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Railway
open Lex
open Syntax

[<TestClass>]
type SyntaxTests() = 
    let shouldFailWith input expected = 
        match Syntax.parse(input) with
            | Failure actual -> Assert.AreEqual(expected, actual)
            | Success actual -> Assert.Fail(sprintf "Expected %A, got %A." expected actual) 

    let shouldParseTo input (expected : Expr list) = 
        match Syntax.parse(input) with
            | Failure actual -> Assert.Fail(actual)
            | Success actual -> // This test isn't strictly necessary, but it does improve the error reporting
                                if actual.Lines.Length <> expected.Length then 
                                    Assert.Fail(sprintf "Expected %A, got %A." expected actual.Lines)
                                let itemMatches exp act = Assert.AreEqual(exp, act, sprintf "Expected %A, got %A." expected act)
                                List.iter (fun (e, a) -> itemMatches e a) (List.zip expected actual.Lines)

    [<TestMethod>]
    member x.``should parse 11`` () = 
        [Token.Number(11)] |> shouldParseTo <| [ Expr.Number(11) ]

    [<TestMethod>]
    member x.``should error on garbage`` () = 
        [Symbol('x')] |> shouldFailWith <| "Identifier expected"

    [<TestMethod>]
    member x.``should parse 11 + 22`` () = 
        [Token.Number(11); Symbol('+'); Token.Number(22)] |> shouldParseTo <| [ Expr.Binary(Expr.Number(11), Operator.Add, Expr.Number(22)) ]

    [<TestMethod>]
    member x.``should parse 2 * 3`` () = 
        [Token.Number(2); Symbol('*'); Token.Number(3)] |> shouldParseTo <| [ Expr.Binary(Expr.Number(2), Operator.Multiply, Expr.Number(3)) ]

    [<TestMethod>]
    member x.``(10 - 2 * 3 should fail with mismatched (`` () = 
        [Symbol('('); Token.Number(10); Symbol('-'); Token.Number(2); Symbol('*'); Token.Number(3)] 
            |> shouldFailWith <| "')' expected."
            
    [<TestMethod>]
    member x.``should parse -1`` () = 
        [Symbol('-'); Token.Number(1)] |> shouldParseTo <| [ Expr.Minus(Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x + 1`` () = 
        [Identifier("x"); Symbol('='); Token.Number(1); NewLine; Identifier("x"); Symbol('+'); Token.Number(1)] 
            |> shouldParseTo <| [ Expr.Assign(Expr.Variable("x"), Number(1)); Expr.Binary(Expr.Variable("x"), Operator.Add, Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x() + 1`` () = 
        [Identifier("x"); Symbol('('); Symbol(')'); Symbol('+'); Token.Number(1)] 
            |> shouldParseTo <| [ Expr.Binary(Expr.Invoke("x"), Operator.Add, Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x = 1``() =
        [Identifier("x"); Symbol('='); Token.Number(1)] |> shouldParseTo <| [ Expr.Assign(Expr.Variable("x"), Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse multiple lines``() =
        [Token.Number(1); NewLine; Token.Number(2)] |> shouldParseTo <| [ Expr.Number(1); Expr.Number(2) ]

    [<TestMethod>]
    member x.``should fail with undeclared local``() =
        [Token.Identifier("x")] |> shouldFailWith <| "Variable \"x\" not declared"

    [<TestMethod>]
    member x.``1 = 2 should fail``() =
        [Token.Number(1); Symbol('='); Token.Number(2)] |> shouldFailWith <| "Unexpected token: Symbol '='"