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

    let parse(tokens: seq<Token>) =
        let expr tokens = 
            Term(Factor(Digit(1)))
        expr tokens 