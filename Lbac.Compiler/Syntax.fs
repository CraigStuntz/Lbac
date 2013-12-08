module Syntax
    
    open Errors
    open Lex

    type Operator =
        | Add
        | Subtract
        | Multiply
        | Divide

    type Expr =
        | Number   of int
        | Variable of string
        | Invoke   of string
        | Minus    of Expr
        | Assign   of Expr * Expr
        | Binary   of Expr * Operator * Expr

    type Line = Try<Expr, string>

    type ParseResult = { Lines: Line list; Locals: Set<string> } 

    /// Converts token list to Success(AST) if valid or Error if not
    let rec parseLine (acc: ParseResult) (tokens: Token list): ParseResult =

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

        /// Returns Success(Expr.Assign(name, right)) when (name is a variable reference and right is Success
        /// Returns Error if any are Error
        let toAssignExpr = function
            | Success name, Success right -> Success(Expr.Assign(name, right))
            | (n, r) -> 
                let errorMessage = function
                    | Error msg -> Some(msg)
                    | _ -> None
                Error([errorMessage(n); errorMessage(r)] 
                    |> List.choose (fun elem -> elem) 
                    |> String.concat "; " )
                    
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
        let rec factor acc = function
            | Symbol '(' :: rest   -> 
                match expression acc rest with 
                | exp, Symbol ')' :: rest', acc' -> exp, rest', acc'
                | _, rest', acc'                 -> Error("')' expected."), rest', acc'
            | Token.Number n :: ts -> Success(Number(n)), ts, acc
            | tokens               -> ident acc tokens

        /// ident = function() | variable
        and ident acc = function
            | Identifier id :: rest ->
                match rest with 
                | Symbol '(' :: rest' -> // function invocation
                    match rest' with 
                    // No support for argument passing yet. 
                    // Only valid function invocation is empty parens: foo()
                    | Symbol ')' :: rest'' -> Success(Invoke(id)), rest'', acc
                    | _                    -> Error ("')' expected"), rest', acc
                | _                   -> // dereference       
                    match acc.Locals.Contains(id) with
                    | true  -> Success(Variable(id)), rest, acc
                    | false -> Error(sprintf "Variable %A not declared" id), rest, acc
            | _ -> Error("Identifier expected"), [], acc
            
        /// term ::= factor  [ mulop factor ]*
        and term acc (tokens: Token list) = 
            let left, rightTokens, acc' = factor acc tokens
            match rightTokens, toMulOp rightTokens with
                | mulOpSym :: ts, Some mulOp -> 
                    let right, rest, acc'' = expression acc' ts
                    toBinaryExpr(left, Success(mulOp), right), rest, acc''
                | _ -> left, rightTokens, acc'

        and unary acc = function
            | Symbol '-' :: rest -> 
                match term acc rest with
                | Success e, rest', acc' -> Success(Minus(e)), rest', acc'
                | error,     rest', _    -> error, rest', acc
            | tokens -> term acc tokens

        and assign acc = function 
            | Identifier name :: rest ->
                match rest with
                | Symbol('=') :: rest' -> 
                    let rhs, rest'', acc' = expression { acc with Locals = acc.Locals.Add(name) } rest'
                    Some(toAssignExpr(Success(Variable(name)), rhs), rest'', acc')
                | _ -> None
            | _ -> None
                
        /// expression ::= [addop] term [addop term]* 
        and expression acc tokens = 
            match assign acc tokens with 
                | Some assignment -> assignment
                | None -> 
                    let leftExpr, rightTokens, acc' = unary acc tokens
                    match rightTokens, toAddOp rightTokens with
                        | addOpSym :: ts, Some addOp -> 
                            let right, rest, acc'' = expression acc' ts
                            toBinaryExpr(leftExpr, Success(addOp), right), rest, acc''
                        | _ -> leftExpr, rightTokens, acc'

        let ast, rest, acc' = expression acc tokens 

        match rest with 
        | []                     -> { acc' with Lines = acc.Lines @ [ast] } // done!
        | NewLine _ :: nextLines -> parseLine { acc' with Lines = acc.Lines @ [ast] } nextLines 
        // If anything remains on line, it's a syntax error
        | wrong     :: _         -> { acc' with Lines = [ Error("Unexpected token: " + (sprintf "%A" wrong)) ] }

    let parse (tokens: Token list): ParseResult =
        parseLine { Lines = []; Locals = Set.empty } tokens 