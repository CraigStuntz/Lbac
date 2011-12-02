namespace Lbac

    open System
    open System.IO
    open System.Collections

    type ExpressionParsing(keyReader: unit -> char, output : TextWriter) = 
        inherit Cradle(keyReader, output)
        
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
            x.term()
            x.emitLn("ADD (SP)+,D0")

        member x.subtract() =
            x.matchChar('-')
            x.term()
            x.emitLn("SUB (SP)+,D0")
            x.emitLn("NEG D0")

//        member x.factor() = 
//            // <factor> ::= <number> -- changed later!
//            x.emitLn(String.Format("MOVE #{0},D0", x.getNum()))

        member x.multiply() =
            x.matchChar('*')
            x.factor()
            x.emitLn("MULS (SP)+,D0")

        member x.divide() =
            x.matchChar('/')
            x.factor()
            x.emitLn("MOVE (SP)+,D1")
            x.emitLn("DIVS D1,D0")

        member x.term() = 
            // <term> ::= <factor>  [ <mulop> <factor> ]*
            x.factor()
            let mulops = set [ '*'; '/' ]
            while Set.contains x.look mulops do
                x.emitLn("MOVE D0,-(SP)")
                match x.look with
                | '*' -> x.multiply()
                | '/' -> x.divide()
                | _   -> x.expected("Mulop")            

        member x.factor() = 
            // <factor> ::= (<expression>)
            match x.look with
            | '(' -> x.matchChar('('); x.expression(); x.matchChar(')'); 
            | _   -> x.emitLn(String.Format("MOVE #{0},D0", x.getNum()))


        member x.expression() = 
            // <expression> ::= [<addop>] <term> [<addop> <term>]*
            if isAddop x.look then
                x.emitLn("CLR D0")
            else
                x.term()
            while isAddop x.look do
                x.emitLn("MOVE D0,-(SP)")
                match x.look with
                | '+' -> x.add()
                | '-' -> x.subtract()
                | _   -> x.expected("Addop")
