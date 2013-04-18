namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Errors
open Lex
open Syntax

[<TestClass>]
type SyntaxTests() = 
    [<TestMethod>]
    member x.``should parse 12`` () = 
        let expected = Try<Expr, string>.Success(Expr.Number(11))
        let input = [Token.Number(11)]
        let actual = Syntax.parse(input)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member x.``should error on garbage`` () = 
        let expected = Try<Expr, string>.Error("Number expected") 
        let input = [Symbol('x')]
        let actual = Syntax.parse(input)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member x.``should parse 11 + 22`` () = 
        let expected = Try<Expr, string>.Success(Expr.Binary(Expr.Number(11), Operator.Add, Expr.Number(22)))
        let input = [Token.Number(11); Symbol('+'); Token.Number(22)]
        let actual = Syntax.parse(input)
        Assert.AreEqual(expected, actual)
