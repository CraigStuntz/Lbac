﻿namespace Lbac.Tests

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