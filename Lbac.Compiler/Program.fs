module Lbac.Program 
    
open System
open System.IO
open IL

let run(reader) = 
    let parser = new Lbac.ExpressionParsing(reader)
    try 
        parser.compile()
    with
        | SyntaxException(m) -> eprintfn "Error: %s" m
                                List.empty<instruction>

let runInteractive() = 
    let il = run(Console.In)
    printfn "%A" il
    Console.ReadLine() |> ignore

let runWithFiles inFile outFile = 
    let input = File.ReadAllText(inFile)
    let reader = new StringReader(input)
    let il = run(reader)
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