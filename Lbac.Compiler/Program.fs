module Lbac.Program 
    
open System
open System.IO
open IL
open Compiler
open Railway

let runInteractive() =
    let input = Console.ReadLine() 
    match Compiler.toIl input with
    | Success il -> printfn "%A" il
    | Failure s    -> Console.Error.WriteLine s
    Console.ReadLine() |> ignore

let runWithFiles inFile outFile = 
    let input = File.ReadAllText(inFile)
    match Compiler.toIl input with
    | Success methodWithInstructions -> 
        let moduleName = match outFile with
                         | Some s -> s
                         | None   -> IO.Path.ChangeExtension(inFile, ".exe")
        let (t, ab) = IL.compileMethod moduleName methodWithInstructions.Instructions typeof<int>
        ab.Save(t.Module.ScopeName) |> ignore        
    | Failure s    -> Console.Error.WriteLine s

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