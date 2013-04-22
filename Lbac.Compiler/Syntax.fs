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
        let toAddOp = function
            | Token.Symbol('+') -> Some(Add)
            | Token.Symbol('-') -> Some(Subtract)
            | _ -> None

        let toMulOp = function
            | Token.Symbol('*') -> Some(Multiply)
            | Token.Symbol('/') -> Some(Divide)
            | _ -> None

        let toBinaryExpr = function
            | Success left, Success oper, Success right -> Success(Expr.Binary(left, oper, right))
            | (l, o, r) -> 
                let errorMessage = function
                    | Error msg -> Some(msg)
                    | _ -> None
                Error([errorMessage(l); errorMessage(r)] 
                    |> List.choose (fun elem -> elem) 
                    |> String.concat "; " )

        let factor = function
            | Token.Number n :: ts -> Success(Number(n)), ts
            | l                    -> Error("Number expected"), l
            
        let rec term (tokens: Token list) = 
            let left, rightTokens = factor tokens
            match rightTokens with
                | s :: ts -> 
                    match toMulOp s with
                    | Some mulOp -> let right, rest = expr(ts) in toBinaryExpr(left, Success(mulOp), right), rest
                    | None -> left, rightTokens
                | _ -> left, rightTokens

        and expr tokens = 
            let left, rightTokens = term tokens
            match rightTokens with
                | s :: ts -> 
                    match toAddOp s with
                    | Some addOp -> let right, rest = expr(ts) in toBinaryExpr(left, Success(addOp), right), rest
                    | None -> left, rightTokens
                | _ -> left, rightTokens

        let ast, rest = expr tokens 
        match rest with 
        | [] -> ast
        | wrong :: _  -> Error("Unexpected token: " + (sprintf "%A" wrong))