namespace Lbac.Tests

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System.IO
    open System.Text
    open Lbac

    [<TestClass>]
    type CradleTests() = class
        [<TestMethod>]
        member x.testInit() = 
            let actual = new StringBuilder()
            let sw = new StringWriter(actual)

            let cradle = new Cradle(new StringReader("1"), sw)

            // the introduction "cradle" code doesn't actually do anything.
            Assert.AreEqual("", actual.ToString())
    end




