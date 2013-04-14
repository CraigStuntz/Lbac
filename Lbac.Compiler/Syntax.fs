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
            | Token.Number n -> Parsed(Number(n))
            | _              -> Error("Number expected")

        let factor = function
            | Number n :: ts -> Parsed(Number(n))
            | _              -> Error("Number expected")
            
        let toAddOp = function
            | '+' -> Some(Operator.Add)
            | '-' -> Some(Operator.Subtract)
            | _   -> None

        let rec parseAddOp (leftTerm, symbol, rest) = 
            match toAddOp symbol with
            | Some addOp -> 
                match expr rest with 
                | Parsed rightTerm -> Parsed(Expr.Binary(leftTerm, addOp, rightTerm))
                | error            -> error
            | None -> Error("+ or - expected here.")

        and expr = function
            | token :: tokens -> 
                match term(token) with
                | Parsed(exp) -> 
                    match tokens with
                    | [] -> Parsed(exp)
                    | Symbol s :: rest -> parseAddOp(exp, s, rest)
                    | wrong    :: _    -> Error("Unexpected token " + wrong.ToString())
                | error -> error
            | _ -> Error("Expression expected")

        expr tokens 