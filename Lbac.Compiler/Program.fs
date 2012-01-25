module Lbac.Program 
    
open System
open System.IO

let run(reader, writer) = 
    let parser = new Lbac.ExpressionParsing(reader, writer)
    parser.expression()

let runInteractive() = 
    let il = run(Console.In, Console.Error)
    printfn "%A" il
    Console.ReadLine() |> ignore


let runWithFiles inFile outFile = 
    let input = File.ReadAllText(inFile)
    let reader = new StringReader(input)
    let il = run(reader, Console.Error)
    let moduleName = match outFile with
                     | Some s -> s
                     | None   -> IO.Path.ChangeExtension(inFile, ".exe")
    let (t, ab) = IL.compileMethod moduleName il typeof<int>
    ab.Save(t.Module.ScopeName) |> ignore

[<EntryPoint>]
let main(args) = 
    let arguments = CommandLine.parse(Array.append [|"Lbac.Compiler.exe"|]  args, Console.Out, Console.Error)
    if arguments.Valid then 
        match arguments.InFile with
        | Some filename -> runWithFiles filename arguments.OutFile
        | _             -> runInteractive()
        0
    else
        1