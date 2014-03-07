module Lex
    open System

    type Token =
        | Identifier of string
        | Number of int
        | Symbol of char
        | NewLine

    let tokenize (input: string) =
        let charIsCrlf c = Set.contains c (set ['\r'; '\n'])
        let rec readIdentifier acc = function
            | c :: rest when Char.IsLetterOrDigit(c) ->
                readIdentifier (acc + c.ToString()) rest
            | rest -> Identifier(acc), rest
        let rec readNumber acc = function 
            | d :: rest when Char.IsDigit(d) ->
                readNumber (acc + d.ToString()) rest
            | rest -> Number(Int32.Parse(acc)), rest
        let rec tokenizeLine acc = function 
            | n :: rest when charIsCrlf n     -> 
                match acc with 
                    | [] -> acc, rest
                    | _  -> (NewLine :: acc), rest
            | d :: rest when Char.IsDigit(d)  ->
                let num, rest' = readNumber (d.ToString()) rest
                tokenizeLine (num :: acc) rest'
            | c :: rest when Char.IsLetter(c) ->
                let ident, rest' = readIdentifier (c.ToString()) rest
                tokenizeLine (ident ::acc) rest'
            | [] -> acc, []
            | ws :: rest when Char.IsWhiteSpace(ws) -> tokenizeLine acc rest
            | c :: rest -> tokenizeLine (Symbol(c) :: acc) rest
        let rec beginningOfLine acc input =
            match tokenizeLine [] input with 
            | tokens, [] -> tokens @ acc
            | acc', rest -> (beginningOfLine [] rest) @ acc' @acc
        List.rev (beginningOfLine [] (List.ofSeq input))
