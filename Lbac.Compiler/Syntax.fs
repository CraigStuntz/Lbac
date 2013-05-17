module Syntax
    
    open Errors
    open Lex

    type Operator =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Assign

    type Expr =
        | Number of int
        | Variable of string
        | Invoke of string
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

        /// factor ::= (expression) | number | ident
        let rec factor = function
            | Symbol '(' :: ts     -> 
                match expression ts with 
                | exp, Symbol ')' :: rest' -> exp, rest'
                | _, rest                  -> Error("')' expected."), rest
            | Token.Number n :: ts -> Success(Number(n)), ts
            | tokens               -> ident tokens

        /// ident = function() | variable
        and ident = function
            | Identifier id :: rest ->
                match rest with 
                | Symbol '(' :: rest' ->
                    match rest' with 
                    | Symbol ')' :: rest'' -> Success(Invoke(id)), rest''
                    | _                    -> Error ("')' expected"), rest'
                | _ -> Success(Variable(id)), rest
            | _ -> Error("Identifier expected"), []
            
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

        and assign = function 
            | Identifier name :: ts ->
                match ts with
                | Symbol('=') :: rest -> 
                    let rhs, remaining = expression(rest)
                    Some(toBinaryExpr(Success(Variable(name)), Success(Operator.Assign), rhs), remaining)
                | _ -> None
            | _ -> None
                
        /// expression ::= [addop] term [addop term]* (* unary negation, + not yet implemented *) 
        and expression tokens = 
            match assign tokens with 
                | Some assignment -> assignment
                | None -> 
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