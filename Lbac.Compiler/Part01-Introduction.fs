namespace Lbac

    open System
    open System.IO

    type Cradle(input : TextReader, errorWriter : TextWriter) =

        let tab = "\t"

        let error(s : string) = 
            errorWriter.WriteLine()
            errorWriter.WriteLine("Error: {0}.", s)

        let abort(s : string) =
            error(s)
            System.Environment.Exit(-1)

        let getChar() = input.Read() |> ignore

        member x.look
            with get() = input.Peek() |> char

        member x.expected(s: string) = 
            abort(s + " Expected")

        // Crenshaw calls this "Match", but match is reserved in F#
        member x.matchChar(c : char) = 
            match x.look with 
            | c when x.look = c -> getChar()
            | _                 -> x.expected(c.ToString())

        member x.getName() =
            if Char.IsLetter(x.look) then
                let c = Char.ToUpperInvariant(x.look)
                getChar()
                c
            else
                failwith "Expected Name"

        member x.getNum() =
            if Char.IsNumber(x.look) then
                let c = x.look
                getChar()
                System.Int32.Parse(string(c))
            else
                failwith "Expected Integer"
