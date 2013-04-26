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
        | Minus of Expr
        | Binary of Expr * Operator * Expr

    type ParseResult = Try<Expr, string>

    /// Converts token list to Success(AST) if valid or Error if not
    let parse(tokens: Token list) =

        /// Returns Some(oper) if head of input is a + or - token
        let toAddOp = function
            | Token.Symbol('+') :: _ -> Some(Add)
            | Token.Symbol('-') :: _ -> Some(Subtract)
            | _                      -> None

        /// Returns Some(oper) if head of input is a * or / token
        let toMulOp = function
            | Token.Symbol('*') :: _ -> Some(Multiply)
            | Token.Symbol('/') :: _ -> Some(Divide)
            | _                      -> None

        /// Returns Success(Expr.Binary(left, oper, right)) when (left, oper, right) are all Success
        /// Returns Error if any are Error
        let toBinaryExpr = function
            | Success left, Success oper, Success right -> Success(Expr.Binary(left, oper, right))
            | (l, o, r) -> 
                let errorMessage = function
                    | Error msg -> Some(msg)
                    | _ -> None
                Error([errorMessage(l); errorMessage(r)] 
                    |> List.choose (fun elem -> elem) 
                    |> String.concat "; " )

        /// factor ::= (expression) | number
        let rec factor = function
            | Symbol '(' :: ts     -> 
                match expression ts with 
                | exp, Symbol ')' :: rest' -> exp, rest'
                | _, rest                  -> Error("')' expected."), rest
            | Token.Number n :: ts -> Success(Number(n)), ts
            | l                    -> Error("Number expected"), l
            
        /// term ::= factor  [ mulop factor ]*
        and term (tokens: Token list) = 
            let left, rightTokens = factor tokens
            match rightTokens, toMulOp rightTokens with
                | mulOpSym :: ts, Some mulOp -> 
                    let right, rest = expression ts
                    toBinaryExpr(left, Success(mulOp), right), rest
                | _ -> left, rightTokens

        and unary = function
            | Symbol '-' :: ts -> 
                match term ts with
                | Success e, rest -> Success(Minus(e)), rest
                | error, rest -> error, rest
            | tokens -> term tokens
                
        /// expression ::= [addop] term [addop term]* (* unary negation, + not yet implemented *) 
        and expression tokens = 
            let left, rightTokens = unary tokens
            match rightTokens, toAddOp rightTokens with
                | addOpSym :: ts, Some addOp -> 
                    let right, rest = expression ts
                    toBinaryExpr(left, Success(addOp), right), rest
                | _ -> left, rightTokens

        // for the time being we can only parse a single expression
        // this will change, but, for now, do that:
        let ast, rest = expression tokens 

        // If anything remains, it's a syntax error
        match rest with 
        | [] -> ast
        | wrong :: _  -> Error("Unexpected token: " + (sprintf "%A" wrong))