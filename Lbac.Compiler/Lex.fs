module Lex
    open System

    type Token =
        | Number of int
        | Symbol of char

    let tokenize (input: string) = 
        let rec readNumber acc = function 
            | d :: rest when Char.IsDigit(d) ->
                readNumber (acc + d.ToString()) rest
            | rest -> Int32.Parse(acc), rest
        let rec tokenize' acc = function 
            | d :: rest when Char.IsDigit(d) ->
                let n, rest' = readNumber (d.ToString()) rest
                tokenize' (Number(n) :: acc) rest'
            | [] -> List.rev acc
            | ws :: rest when Char.IsWhiteSpace(ws) -> tokenize' acc rest
            | c :: rest -> tokenize' (Symbol(c) :: acc) rest
        tokenize' [] (List.ofSeq input)
