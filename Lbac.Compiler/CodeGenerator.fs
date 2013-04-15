module CodeGenerator

    open IL
    open Syntax

    let private codegen_oper = function
        | Add -> instruction.Add
        | Subtract -> instruction.Sub
        | Multiply -> instruction.Mul
        | Divide -> instruction.Div

    let rec codegen = function
        | Number n -> 
            match n with
            | 0 -> [Ldc_I4_0]
            | _ -> [Ldc_I4 n]
        | Binary (lhs, oper, rhs) -> 
            let lhsIl = codegen lhs
            let rhsIl = codegen rhs
            let operInst = codegen_oper oper
            List.concat [ lhsIl; rhsIl; [operInst] ]

        

