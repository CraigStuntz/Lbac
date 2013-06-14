namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Errors
open Lex
open Syntax

[<TestClass>]
type SyntaxTests() = 
    let lineShouldFailWith input expected = 
        let actual = Syntax.parse(input)
        let isMatch = function 
            | Error e -> expected = e
            | _ -> false
        if not (List.exists isMatch actual) then Assert.Fail("Expected " + expected) 

    let shouldParseTo input expected = 
        let actual = Syntax.parse(input)
        let itemMatches exp = function
            | Success parsed -> Assert.AreEqual(exp, parsed)
            | Error e -> Assert.Fail e
        List.iter (fun (e, a) -> itemMatches e a) (List.zip expected actual)

    [<TestMethod>]
    member x.``should parse 11`` () = 
        [Token.Number(11)] |> shouldParseTo <| [ Expr.Number(11) ]

    [<TestMethod>]
    member x.``should error on garbage`` () = 
        [Symbol('x')] |> lineShouldFailWith <| "Identifier expected"

    [<TestMethod>]
    member x.``should parse 11 + 22`` () = 
        [Token.Number(11); Symbol('+'); Token.Number(22)] |> shouldParseTo <| [ Expr.Binary(Expr.Number(11), Operator.Add, Expr.Number(22)) ]

    [<TestMethod>]
    member x.``should parse 2 * 3`` () = 
        [Token.Number(2); Symbol('*'); Token.Number(3)] |> shouldParseTo <| [ Expr.Binary(Expr.Number(2), Operator.Multiply, Expr.Number(3)) ]

    [<TestMethod>]
    member x.``(10 - 2 * 3 should fail with mismatched (`` () = 
        [Symbol('('); Token.Number(10); Symbol('-'); Token.Number(2); Symbol('*'); Token.Number(3)] 
            |> lineShouldFailWith <| "')' expected."
            
    [<TestMethod>]
    member x.``should parse -1`` () = 
        [Symbol('-'); Token.Number(1)] |> shouldParseTo <| [ Expr.Minus(Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x + 1`` () = 
        [Identifier("x"); Symbol('+'); Token.Number(1)] 
            |> shouldParseTo <| [ Expr.Binary(Expr.Variable("x"), Operator.Add, Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x() + 1`` () = 
        [Identifier("x"); Symbol('('); Symbol(')'); Symbol('+'); Token.Number(1)] 
            |> shouldParseTo <| [ Expr.Binary(Expr.Invoke("x"), Operator.Add, Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse x = 1``() =
        [Identifier("x"); Symbol('='); Token.Number(1)] |> shouldParseTo <| [ Expr.Binary(Expr.Variable("x"), Operator.Assign, Expr.Number(1)) ]

    [<TestMethod>]
    member x.``should parse multiple lines``() =
        [Token.Number(1); NewLine; Token.Number(2)] |> shouldParseTo <| [ Expr.Number(1); Expr.Number(2) ]