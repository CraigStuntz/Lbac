namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Errors
open Lex
open Syntax

[<TestClass>]
type SyntaxTests() = 
    let shouldFailWith input expected = 
        let actual = Syntax.parse(input)
        match actual with 
        | Success _ -> Assert.Fail("Expected " + expected)
        | Error e -> Assert.AreEqual(expected, e)

    let shouldParseTo input expected = 
        let actual = Syntax.parse(input)
        match actual with 
        | Success parsed -> Assert.AreEqual(expected, parsed)
        | Error e -> Assert.Fail e

    [<TestMethod>]
    member x.``should parse 11`` () = 
        [Token.Number(11)] |> shouldParseTo <| Expr.Number(11)

    [<TestMethod>]
    member x.``should error on garbage`` () = 
        [Symbol('x')] |> shouldFailWith <| "Unexpected token: Symbol 'x'"

    [<TestMethod>]
    member x.``should parse 11 + 22`` () = 
        [Token.Number(11); Symbol('+'); Token.Number(22)] |> shouldParseTo <| Expr.Binary(Expr.Number(11), Operator.Add, Expr.Number(22))

    [<TestMethod>]
    member x.``should parse 2 * 3`` () = 
        [Token.Number(2); Symbol('*'); Token.Number(3)] |> shouldParseTo <| Expr.Binary(Expr.Number(2), Operator.Multiply, Expr.Number(3))


    [<TestMethod>]
    member x.``(10 - 2 * 3 should fail with mismatched (`` () = 
        [Symbol('('); Token.Number(10); Symbol('-'); Token.Number(2); Symbol('*'); Token.Number(3)] 
            |> shouldFailWith <| "')' expected."
