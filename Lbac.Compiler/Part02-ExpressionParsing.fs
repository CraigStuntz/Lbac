namespace Lbac

    open System
    open System.IO
    open IL

    type ExpressionParsing(input : TextReader, errorWriter : TextWriter) = 
        inherit Cradle(input, errorWriter)
        
// Introduced later, but F# likes it above members.

        let isAddop(c) = 
            let addops = set [ '+'; '-' ]
            Set.contains c addops

//        Crenshaw starts Chapter 2 by calling this method "Expression" and then
//        changes the name to "Term" later on. This is replaced when we get to *,/
//        member x.term() = 
//            x.emitLn(String.Format("MOVE #{0},D0", x.getNum()))

//        This method is changed later in Chapter 2. 
//        This version can only handle expressions like "1+2" or "2-1"
//        member x.expression() = 
//            x.term()
//            x.emitLn("MOVE D0,D1")
//            match x.look with
//            | '+' -> x.add()
//            | '-' -> x.subtract()
//            | _   -> x.expected("Addop")

//        This version can handle 1+3-2; again, it's replaced below with a "stack" version
//        member x.expression() = 
//            x.term()
//            let arithmeticOperators = set [ '+'; '-' ]
//            while Set.contains x.look arithmeticOperators do
//                x.emitLn("MOVE D0,D1")
//                match x.look with
//                | '+' -> x.add()
//                | '-' -> x.subtract()
//                | _   -> x.expected("Addop")

//        Replaced later with "stack" version            
//        member x.add() =
//            x.matchChar('+')
//            x.term()
//            x.emitLn("ADD D1,D0")

//        This first "buggy" subtract is replaced later in the chapter
//        member x.subtract() =
//            x.matchChar('-')
//            x.term()
//            x.emitLn("SUB D1,D0")

//        Non-buggy version also replaced later with "stack" version            
//        member x.subtract() =
//            x.matchChar('-')
//            x.term()
//            x.emitLn("SUB D1,D0")
//            x.emitLn("NEG D0")

//        member x.expression() = 
//            // <expression> ::= <term> [<addop> <term>]*
//            x.term()
//            let addops = set [ '+'; '-' ]
//            while Set.contains x.look addops do
//                x.emitLn("MOVE D0,-(SP)")
//                match x.look with
//                | '+' -> x.add()
//                | '-' -> x.subtract()
//                | _   -> x.expected("Addop")

        member x.add() =
            x.matchChar('+')
            x.term() @ [ IL.Add ]

        member x.subtract() =
            x.matchChar('-')
            x.term() @ [ IL.Sub ]

//        member x.factor() = 
//            // <factor> ::= <number> -- changed later!
//            x.emitLn(String.Format("MOVE #{0},D0", x.getNum()))

        member x.multiply() =
            x.matchChar('*')
            x.factor() @ [ IL.Mul ]

        member x.divide() =
            x.matchChar('/')
            x.factor() @ [ IL.Div ]

        member x.term() = 
            // <term> ::= <factor>  [ <mulop> <factor> ]*
            let mutable result = x.factor()
            let mulops = set [ '*'; '/' ]
            while Set.contains x.look mulops do
                match x.look with
                    | '*' -> result <- result @ x.multiply()
                    | '/' -> result <- result @ x.divide()
                    | _   -> x.expected("Mulop") 
            result

        member x.factor() : list<IL.instruction> = 
            // <factor> ::= (<expression>)
            if x.look = '(' then
                x.matchChar('(')
                let expr = x.expression()
                x.matchChar(')')
                expr
            else
                [IL.Ldc_I4(x.getNum())]

        member x.expression() = 
            // <expression> ::= [<addop>] <term> [<addop> <term>]*
            let mutable result = if isAddop x.look then
                                    [IL.Ldc_I4_0]
                                 else
                                    x.term()
            while isAddop x.look do
                match x.look with
                | '+' -> result <- result @ x.add()
                | '-' -> result <- result @ x.subtract()
                | _   -> x.expected("Addop")
            result
