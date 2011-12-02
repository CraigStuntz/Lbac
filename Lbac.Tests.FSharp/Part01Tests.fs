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
            let kr() = '1';

            let cradle = new Cradle(kr, sw)

            // the introduction "cradle" code doesn't actually do anything.
            Assert.AreEqual("", actual.ToString())
    end




