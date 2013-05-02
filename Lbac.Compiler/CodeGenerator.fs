module CodeGenerator

    open Errors
    open IL
    open Syntax

    let private codegen_oper = function
        | Add -> instruction.Add
        | Subtract -> instruction.Sub
        | Multiply -> instruction.Mul
        | Divide -> instruction.Div

    let rec codegen = function
        | Success expr -> 
            match expr with
            | Variable v -> Error("Sorry; no can do")
            | Invoke m -> Error("Sorry; no can do")
            | Minus e -> 
                match codegen(Success(e)) with
                | Success il -> Success(il @ [Neg])
                | err -> err
            | Number n -> 
                match n with
                | 0 -> Success([Ldc_I4_0])
                | _ -> Success([Ldc_I4 n])
            | Binary (lhs, oper, rhs) -> 
                let lhsIl = codegen(Success(lhs))
                let rhsIl = codegen(Success(rhs))
                let operInst = codegen_oper oper
                match (lhsIl, rhsIl) with
                    | (Success l, Success r) -> Success(List.concat [ l; r; [operInst] ])
                    | (Error l, _) -> lhsIl
                    | (_, Error r) -> rhsIl
        | Error(e) -> Error(e)

        

