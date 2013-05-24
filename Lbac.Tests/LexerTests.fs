namespace Lbac.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Lex

[<TestClass>]
type LexerTests() = 
    [<TestMethod>]
    member x.``should lex 11 + 22`` () = 
        let testVal = "11 + 22"
        let expected = [Number(11); Symbol('+'); Number(22)]
        let actual = Lex.tokenize testVal
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member x.``should lex identifier`` () = 
        let testVal = "foo = 1"
        let expected = [Identifier("foo"); Symbol('='); Number(1)]
        let actual = Lex.tokenize testVal
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member x.``should lex multiple lines`` () = 
        let testVal = "1\n2"
        let expected = [Number(1); NewLine; Number(2)]
        let actual = Lex.tokenize testVal
        Assert.AreEqual(expected, actual)