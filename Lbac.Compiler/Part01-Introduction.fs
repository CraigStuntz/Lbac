namespace Lbac

    open System
    open System.IO
    open IL

    type Cradle(input : TextReader, errorWriter : TextWriter) =

        let tab = "\t"

        let error(s : string) = 
            raise (SyntaxException(s))

        let getChar() = input.Read() |> ignore

        member x.look
            with get() = input.Peek() |> char

        member x.expected(s: string) = 
            error(s + " Expected")

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
                x.expected "Name"

        member x.getNum() =
            if Char.IsNumber(x.look) then
                let c = x.look
                getChar()
                System.Int32.Parse(string(c))
            else
                x.expected "Integer"

        abstract member compile: unit -> instruction list
        default x.compile() = 
            List.empty<instruction>