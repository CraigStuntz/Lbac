module Lex
    open System

    type Token =
        | Identifier of string
        | Number of int
        | Symbol of char

    let tokenize (input: string) =
        let rec readIdentifier acc = function
            | c :: rest when Char.IsLetterOrDigit(c) ->
                readIdentifier (acc + c.ToString()) rest
            | rest -> Identifier(acc), rest
        let rec readNumber acc = function 
            | d :: rest when Char.IsDigit(d) ->
                readNumber (acc + d.ToString()) rest
            | rest -> Number(Int32.Parse(acc)), rest
        let rec tokenize' acc = function 
            | d :: rest when Char.IsDigit(d) ->
                let num, rest' = readNumber (d.ToString()) rest
                tokenize' (num :: acc) rest'
            | c :: rest when Char.IsLetter(c) ->
                let ident, rest' = readIdentifier (c.ToString()) rest
                tokenize' (ident ::acc) rest'
            | [] -> List.rev acc
            | ws :: rest when Char.IsWhiteSpace(ws) -> tokenize' acc rest
            | c :: rest -> tokenize' (Symbol(c) :: acc) rest
        tokenize' [] (List.ofSeq input)
