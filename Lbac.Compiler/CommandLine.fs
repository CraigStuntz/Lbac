module Lbac.CommandLine

    open Microsoft.FSharp.Text

    type Arguments = {
        InFile   : string option
        OutFile  : string option
        Valid    : bool }

    let parse (commandLine : string[], outw : System.IO.TextWriter, error : System.IO.TextWriter) = 
        let usage s = outw.WriteLine "Compiler [-in filename] [-out filename] [-help]"
        let inFile = ref None
        let outFile = ref None
        let mutable valid = true
        let specs = 
            [ "-i",       ArgType.String (fun s  -> inFile := Some s),        "Name of input file"
              "-o",       ArgType.String (fun s  -> outFile := Some s),       "Name of output file"
            ] |> List.map (fun (name, action, help) -> ArgInfo(name, action, help))        
        let current = ref 0
        try ArgParser.ParsePartial(current, commandLine, specs, usage) with
            | Bad e ->
                valid <- false
                error.WriteLine(e)
            | HelpText h -> 
                outw.WriteLine(h)
            | e -> 
                reraise()

        { InFile = inFile.Value; OutFile = outFile.Value; Valid = valid }