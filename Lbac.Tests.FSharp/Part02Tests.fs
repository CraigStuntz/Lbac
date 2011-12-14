namespace Lbac.Tests

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System.IO
    open System.Text
    open Lbac
    open IL

    [<TestClass>]
    type ParsingTests() = class
        let execute input = 
            let actual = new StringBuilder()
            let sw = new StringWriter(actual)
            let sr = new StringReader(input)
            let kr() = 
                let c = sr.Read()
                match c with 
                    | -1 -> 
                    '\u0000'
                    | _  -> System.Convert.ToChar(c);
            let parser = new ExpressionParsing(kr, sw)
            let il = parser.expression()
            IL.execute<System.Int32> (il, false) // change false to true to save assembly to disk -- useful for running PEVerify.

        [<TestMethod>]
        member x.testTerm() = 
            let actual = new StringBuilder()
            let sw = new StringWriter(actual)
            let kr() = '1';
            let parser = new ExpressionParsing(kr, sw)

            let actual = parser.term()
            let arg = match actual.Head with
                      | Ldc_I4 n -> n
                      | _        -> -1

            Assert.AreEqual(1, arg)

        [<TestMethod>]
        member x.testAddition() = 
            let input = "1+2";
            let expected = 3;

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testSubtraction() = 
            let input = "1-2";
            let expected = -1;

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testThreeTermExpression() = 
            let input = "1+2-5"
            let expected = -2

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testMultiply() = 
            let input = "2+3*4";
            let expected = 14;

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testDivide() = 
            let input = "6/2";
            let expected = 3;

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testParenthesizedExpression() = 
            let input = "(1+2)*((3+4)+(5-6))"
            let expected = 18

            let actual = execute input

            Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member x.testUnaryMinus() = 
            let input = "-1"; // compiler should treat as "0-1"
            let expected = -1;

            let actual = execute input

            Assert.AreEqual(expected, actual)

    end


