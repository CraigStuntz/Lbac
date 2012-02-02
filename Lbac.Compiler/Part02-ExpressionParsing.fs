namespace Lbac

    open System
    open System.IO
    open IL

    type ExpressionParsing(input) = 
        inherit Cradle(input)
        
        member x.add() =
            x.matchChar('+')
            x.term() @ [ IL.Add ]

        member x.subtract() =
            x.matchChar('-')
            x.term() @ [ IL.Sub ]

        member x.multiply() =
            x.matchChar('*')
            x.factor() @ [ IL.Mul ]

        member x.divide() =
            x.matchChar('/')
            x.factor() @ [ IL.Div ]

        /// <summary>
        /// <term> ::= <factor>  [ <mulop> <factor> ]*
        /// </summary>
        member x.term() = 
            let head = x.factor()
            let rec termTail head = 
                match x.look with
                    | '*' -> termTail ( head @ x.multiply() )
                    | '/' -> termTail ( head @ x.divide() )
                    | _   -> head 
            termTail head

        /// <summary>
        /// <factor> ::= (<expression>) | <number>
        /// </summary>
        member x.factor() : list<IL.instruction> = 
            match x.look with
            | '(' -> x.matchChar('(')
                     let expr = x.expression()
                     x.matchChar(')')
                     expr
            | _   -> [IL.Ldc_I4(x.getNum())]


        /// <summary>
        /// <expression> ::= [<addop>] <term> [<addop> <term>]*
        /// </summary>
        member x.expression() = 
            let addops = set [ '+'; '-']
            let head = if Set.contains x.look addops then
                           [IL.Ldc_I4_0]
                       else
                           x.term()
            // rest of expression is evaluated recurively for forms like 1+2-3+4...
            let rec expressionTail head = 
                match x.look with
                | '+' -> expressionTail( head @ x.add()      )
                | '-' -> expressionTail( head @ x.subtract() )
                | _   -> head
            expressionTail head

        override x.compile() = 
            x.expression()
