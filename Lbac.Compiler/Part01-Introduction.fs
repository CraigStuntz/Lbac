namespace Lbac

    open System
    open System.IO
    open IL

    type Cradle(input : TextReader) =

        let tab = "\t"

        let error(s : string) = 
            raise (SyntaxException(s))

        let getChar() = input.Read() |> ignore

        member x.look
            with get() = input.Peek() |> char

        member x.expected(s: string) = 
            error(s + " Expected")

        /// <summary>
        /// Crenshaw calls this "Match", but match is reserved in F#
        /// </summary>
        member x.matchChar(c : char) = 
            match x.look with 
            | c when x.look = c -> getChar()
            | _                 -> x.expected(c.ToString())

        member x.getName() =
            match x.look with
            | c when Char.IsLetter(c) -> getChar(); c |> Char.ToUpperInvariant
            | _                       -> x.expected "Name"

        member x.getNum() =
            match x.look with
            | c when Char.IsNumber(c) -> getChar(); c |> string |> System.Int32.Parse
            | _                       -> x.expected "Integer"

        abstract member compile: unit -> instruction list
        default x.compile() = 
            List.empty<instruction>