module Syntax

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