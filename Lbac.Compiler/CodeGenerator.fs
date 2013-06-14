module CodeGenerator

    open Errors
    open IL
    open Syntax

    let private codegen_oper = function
        | Add -> instruction.Add
        | Subtract -> instruction.Sub
        | Multiply -> instruction.Mul
        | Divide -> instruction.Div
        | Assign -> failwith "Sorry; no can do"

    let rec codegenExpr (acc : Method) (expr : Expr) = 
        match expr with
        | Variable v -> Error "Sorry; no can do"
        | Invoke m -> Error "Sorry; no can do"
        | Minus e -> 
            match codegenExpr acc e with
            | Success m -> Success({ m with Instructions = m.Instructions @ [Neg] })
            | err -> err
        | Number n -> 
            match n with
            | 0 -> Success({ acc with Instructions = acc.Instructions @ [Ldc_I4_0] })
            | _ -> Success({ acc with Instructions = acc.Instructions @ [Ldc_I4 n] })
        | Binary (lhs, oper, rhs) -> 
            let lhsMethod = codegenExpr { acc with Instructions = [] } lhs
            let rhsMethod = codegenExpr { acc with Instructions = [] } rhs
            let operInst = codegen_oper oper
            match (lhsMethod, rhsMethod) with
                | (Success l, Success r) -> 
                    let insts       = List.concat [ l.Instructions; r.Instructions; [operInst] ]
                    let mergeLocals = List.concat [ l.Locals; List.filter (fun i2 -> not (List.exists (fun i1 -> i1 = i2) l.Locals)) r.Locals]
                    Success({ Instructions = insts; Locals = mergeLocals })
                | (Error l, _) -> lhsMethod
                | (_, Error r) -> rhsMethod

    let rec codegen (expList : ParseResult list) =
        let tryCodeGenLine acc line = 
            match acc, line with
            | Success accMethod, Success expr -> codegenExpr accMethod expr
            | _, Error err -> Error err
            | Error err, _ -> Error err
        let emptyMethod = Success( { Instructions = List.empty<instruction>; Locals = List.empty<LocalVar> } )
        List.fold tryCodeGenLine emptyMethod expList
