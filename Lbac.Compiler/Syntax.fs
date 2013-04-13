module Syntax
    
    open Lex

    type Operator =
        | Add
        | Subtract
        | Multiply
        | Divide

    type Expr =
        | Number of int
        | Binary of Expr * Operator * Expr

    type ParseResult =
        | Parsed of Expr
        | Error of string

    let parse(tokens: Token list) =
        let term = function
            | Number n :: _ -> Parsed(Number(n))
            | _ -> Error("Number expected")

        let factor = function
            | Number n :: ts -> Parsed(Number(n))
            | _ -> Error("Number expected")
            
        let toAddOp = function
            | '+' -> Some(Add)
            | '-' -> Some(Subtract)
            | _ -> None

        let rec expr = function
            | [Token.Number n] -> Parsed(Number(n))
            | _ -> Error("Expression expected")

        expr tokens 