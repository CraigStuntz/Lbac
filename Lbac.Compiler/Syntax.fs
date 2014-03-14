module Syntax
    
    open Railway
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
        | Error of string

    type ParseResult = { Lines: Expr list; Locals: Set<string> } 

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

        /// factor ::= (expression) | number | ident
        let rec factor acc = function
            | Symbol '(' :: rest   -> 
                match expression acc rest with 
                | exp, Symbol ')' :: rest', acc' -> exp, rest', acc'
                | _, rest', acc'                 -> Error("')' expected."), rest', acc'
            | Token.Number n :: ts -> Number(n), ts, acc
            | tokens               -> ident acc tokens

        /// ident = function() | variable
        and ident acc = function
            | Identifier id :: rest ->
                match rest with 
                | Symbol '(' :: rest' -> // function invocation
                    match rest' with 
                    // No support for argument passing yet. 
                    // Only valid function invocation is empty parens: foo()
                    | Symbol ')' :: rest'' -> Invoke(id), rest'', acc
                    | _                    -> Error ("')' expected"), rest', acc
                | _                   -> // dereference       
                    match acc.Locals.Contains(id) with
                    | true  -> Variable(id), rest, acc
                    | false -> Error(sprintf "Variable %A not declared" id), rest, acc
            | _ -> Error("Identifier expected"), [], acc
            
        /// term ::= factor  [ mulop factor ]*
        and term acc (tokens: Token list) = 
            let left, rightTokens, acc' = factor acc tokens
            match rightTokens, toMulOp rightTokens with
                | mulOpSym :: ts, Some mulOp -> 
                    let right, rest, acc'' = expression acc' ts
                    Binary(left, mulOp, right), rest, acc''
                | _ -> left, rightTokens, acc'

        and unary acc = function
            | Symbol '-' :: rest -> 
                match term acc rest with
                | e, rest', acc' -> Minus(e), rest', acc'
            | tokens -> term acc tokens

        and assign acc = function 
            | Identifier name :: rest ->
                match rest with
                | Symbol('=') :: rest' -> 
                    let rhs, rest'', acc' = expression { acc with Locals = acc.Locals.Add(name) } rest'
                    Some(Assign(Variable(name), rhs), rest'', acc')
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
                            Binary(leftExpr, addOp, right), rest, acc''
                        | _ -> leftExpr, rightTokens, acc'

        let ast, rest, acc' = expression acc tokens 

        match rest with 
        | [] 
        | NewLine :: []    -> { acc' with Lines = acc.Lines @ [ast] } // done!
        | NewLine :: rest' -> parseLine { acc' with Lines = acc.Lines @ [ast] } rest' 
        // If anything remains on line, it's a syntax error
        | wrong   :: _     -> { acc' with Lines = [ Error("Unexpected token: " + (sprintf "%A" wrong)) ] }

    let tryParse (tokens: Token list): ParseResult =
        parseLine { Lines = []; Locals = Set.empty } tokens 

    let errorMessagesFor expr = 
        let rec errorMessagesForImpl acc = function 
            | Minus  expr                -> acc @ errorMessagesForImpl acc expr 
            | Assign (name, value)       -> acc @ errorMessagesForImpl acc value @ errorMessagesForImpl acc name
            | Binary (left, oper, right) -> acc @ errorMessagesForImpl acc right @ errorMessagesForImpl acc left
            | Error  message          -> [message]
            | Number   _ 
            | Variable _ 
            | Invoke   _                 -> acc
        errorMessagesForImpl [] expr

    let errorsForLines (lines: Expr list) =
        List.collect errorMessagesFor lines
        
    let parse (tokens: Token list): Result<ParseResult, string> = 
        let result = tryParse tokens
        match errorsForLines result.Lines with
        | []     -> Success(result)
        | errors -> Failure(System.String.Join(System.Environment.NewLine, errors))