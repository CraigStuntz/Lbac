namespace Lbac.Tests

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System.IO
    open System.Text
    open Lbac

    [<TestClass>]
    type CommandLineTests() = class
        let parse_with_fake_output args = 
            let out = new StringBuilder()
            let error = new StringBuilder()

            let actual = Lbac.CommandLine.parse(args, new StringWriter(out), new StringWriter(error))

            (actual, out.ToString(), error.ToString())

        [<TestMethod>]
        member x.parse_should_find_inFile() = 
            let expected = "foo.txt"
            let args = [| "programname.exe"; "-i"; expected; "-o"; "bar.exe"; "-chapter"; "1" |]

            let (actual, out, error) = parse_with_fake_output args

            Assert.AreEqual(expected, actual.InFile.Value)
            Assert.IsTrue(System.String.IsNullOrEmpty(out))
            Assert.IsTrue(System.String.IsNullOrEmpty(error))
            Assert.IsTrue(actual.Valid)

        [<TestMethod>]
        member x.parse_should_find_outFile() = 
            let expected = "bar.exe"
            let args = [| "programname.exe"; "-i"; "foo.txt"; "-o"; expected; "-chapter"; "1" |]

            let (actual, out, error) = parse_with_fake_output args

            Assert.AreEqual(expected, actual.OutFile.Value)
            Assert.IsTrue(System.String.IsNullOrEmpty(out))
            Assert.IsTrue(System.String.IsNullOrEmpty(error))
            Assert.IsTrue(actual.Valid)

        [<TestMethod>]
        member x.parse_should_find_chapter() = 
            let expected = Some 1
            let args = [| "programname.exe"; "-i"; "foo.txt"; "-o"; "bar.exe"; "-chapter"; "1" |]

            let (actual, out, error) = parse_with_fake_output args

            Assert.AreEqual(expected, actual.Chapter)
            Assert.IsTrue(System.String.IsNullOrEmpty(out))
            Assert.IsTrue(System.String.IsNullOrEmpty(error))
            Assert.IsTrue(actual.Valid)

        [<TestMethod>]
        member x.parse_should_prompt_for_missing_values() = 
            let expected = Some 1
            let args = [| "programname.exe"; "-i" |]

            let (actual, out, error) = parse_with_fake_output args

            Assert.IsFalse(System.String.IsNullOrEmpty(error))
            Assert.IsFalse(actual.Valid)

    end