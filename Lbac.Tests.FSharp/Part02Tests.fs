namespace Lbac.Tests

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System.IO
    open System.Text
    open Lbac

    [<TestClass>]
    type ParsingTests() = class
        let compile(input) = 
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
            parser.expression()
            actual.ToString()

        [<TestMethod>]
        member x.testTerm() = 
            let actual = new StringBuilder()
            let sw = new StringWriter(actual)
            let kr() = '1';
            let parser = new ExpressionParsing(kr, sw)

            parser.term()

            Assert.AreEqual("\tMOVE #1,D0" + System.Environment.NewLine, actual.ToString())

        [<TestMethod>]
        member x.testExpression() = 
            let input = "1+2";
            let expected = System.String.Format("{0}MOVE #1,D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #2,D0{1}{0}ADD (SP)+,D0{1}", '\t', System.Environment.NewLine);

            let actual = compile input

            Assert.AreEqual(expected, actual.ToString())

        [<TestMethod>]
        member x.testThreeTermExpression() = 
            let input = "1+2-5";
            let expected = System.String.Format("{0}MOVE #1,D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #2,D0{1}{0}ADD (SP)+,D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #5,D0{1}{0}SUB (SP)+,D0{1}{0}NEG D0{1}", '\t', System.Environment.NewLine);

            let actual = compile input

            Assert.AreEqual(expected, actual.ToString())

        [<TestMethod>]
        member x.testMultiplyDivide() = 
            let input = "2+3*4";
            let expected = System.String.Format("{0}MOVE #2,D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #3,D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #4,D0{1}{0}MULS (SP)+,D0{1}{0}ADD (SP)+,D0{1}", '\t', System.Environment.NewLine);

            let actual = compile input

            Assert.AreEqual(expected, actual.ToString())

        [<TestMethod>]
        member x.testParenthesizedExpression() = 
            let input = "(1+2)/((3+4)+(5-6))";
            let expectedLastInstruction = System.String.Format("DIVS D1,D0{0}", System.Environment.NewLine);

            let actual = compile input

            // this produces quite a bit of code, so just verify that the DIVS is last. 
            Assert.IsTrue(actual.ToString().EndsWith(expectedLastInstruction))

        [<TestMethod>]
        member x.testUnaryMinus() = 
            let input = "-1"; // compiler should treat as "0-1"
            let expected = System.String.Format("{0}CLR D0{1}{0}MOVE D0,-(SP){1}{0}MOVE #1,D0{1}{0}SUB (SP)+,D0{1}{0}NEG D0{1}", '\t', System.Environment.NewLine);

            let actual = compile input

            Assert.AreEqual(expected, actual.ToString())

    end


