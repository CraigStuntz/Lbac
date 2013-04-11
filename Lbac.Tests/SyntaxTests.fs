namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Lex
open Syntax

[<TestClass>]
type SyntaxTests() = 
    [<TestMethod>]
    member x.``should parse 11 + 22`` () = 
        let expected = Expr.AddOp(Expr.Term(Factor(Digit(11))), Factor(Digit(22)))
        let input = [Number(11); Symbol('+'); Number(22)]
        let actual = Syntax.parse(input)
        Assert.AreEqual(expected, actual)
