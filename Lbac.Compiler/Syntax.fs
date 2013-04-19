module Syntax
    
    open Errors
    open Lex

    type Operator =
        | Add
        | Subtract
        | Multiply
        | Divide

    type Expr =
        | Number of int
        | Binary of Expr * Operator * Expr

    type ParseResult = Try<Expr, string>

    let parse(tokens: Token list) =
        let term = function
            | Token.Number n -> Success(Number(n))
            | _              -> Error("Number expected")

        let factor = function
            | Number n :: ts -> Success(Number(n))
            | _              -> Error("Number expected")
            
        let toAddOp = function
            | '+' -> Some(Operator.Add)
            | '-' -> Some(Operator.Subtract)
            | _   -> None

        let toMulOp = function
            | '*' -> Some(Operator.Multiply)
            | '/' -> Some(Operator.Divide)
            | _   -> None

        let rec parseAddOp (leftTerm, symbol, rest) = 
            match toAddOp symbol with
            | Some addOp -> 
                match expr rest with 
                | Success rightTerm -> Success(Expr.Binary(leftTerm, addOp, rightTerm))
                | error            -> error
            | None -> Error("+ or - expected here.")

        and parseMulOp (leftTerm, symbol, rest) = 
            match toMulOp symbol with
            | Some mulOp -> 
                match expr rest with 
                | Success rightTerm -> Success(Expr.Binary(leftTerm, mulOp, rightTerm))
                | error            -> error
            | None -> Error("* or / expected here.")

        and expr = function
            | token :: tokens -> 
                match term(token) with
                | Success exp -> 
                    match tokens with
                    | [] -> Success(exp)
                    | Symbol s :: rest -> parseAddOp(exp, s, rest)
                    | wrong    :: _    -> Error("Unexpected token " + wrong.ToString())
                | error -> error
            | _ -> Error("Expression expected")

        expr tokens 