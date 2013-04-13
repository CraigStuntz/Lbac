module Syntax
    
    open Lex

    type Factor = 
        | Digit of int
        | ParenExpr of Expr

    and AddOp =
        | Plus 
        | Minus

    and MulOp =
        | Times
        | DividedBy

    and Term =
        | MulOp of Term * Factor
        | Factor of Factor

    and Expr = 
        | AddOp of Expr * Term
        | Term of Term

    type Parsed =
        | Some of Expr
        | Error of string

    let parse(tokens: Token list) =
        let term = function
            | Number n :: _ -> Some(Expr.Term(Factor(Digit(n))))
            | _ -> Error("Number expected")

        let factor = function
            | Number n :: ts -> Some(Expr.Term(Factor(Digit(n))))
            | _ -> Error("Number expected") 

        let expr tokens = 
            term tokens

        expr tokens 