module Lbac.Program 
    
open System
open System.IO

let run(reader, writer) = 
    let parser = new Lbac.ExpressionParsing(reader, writer)
    parser.expression()

let runInteractive() = 
    let outputStream = Console.Out
    let keyReader() = 
        let cki = Console.ReadKey()
        cki.KeyChar
    let il = run(keyReader, outputStream)
    printfn "%A" il
    Console.ReadLine() |> ignore


let runWithFiles inFile outFile = 
    let outputStream = Console.Out
    let input = File.ReadAllText(inFile)
    let reader = new StringReader(input)
    let charReader() = 
        Convert.ToChar(reader.Read())    
    let il = run(charReader, outputStream)
    let moduleName = match outFile with
                     | Some s -> s
                     | None   -> IO.Path.ChangeExtension(inFile, ".exe")
    let (t, ab) = IL.compileMethod moduleName il typeof<int>
    ab.Save(t.Module.ScopeName) |> ignore

[<EntryPoint>]
let main(args) = 
    let arguments = CommandLine.parse(Array.append [|"Lbac.Compiler.exe"|]  args, Console.Out, Console.Error)
    if arguments.Valid then 
        if arguments.InFile.IsSome then
            runWithFiles arguments.InFile.Value arguments.OutFile
        else 
            runInteractive()
        0
    else
        1