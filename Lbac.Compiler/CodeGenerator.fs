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

    let rec codegenExpr : Expr -> Try<Method, string> = function
        | Variable v -> Error "Sorry; no can do"
        | Invoke m -> Error "Sorry; no can do"
        | Minus e -> 
            match codegenExpr e with
            | Success m -> Success({ m with Instructions = m.Instructions @ [Neg] })
            | err -> err
        | Number n -> 
            match n with
            | 0 -> Success({ Instructions = [Ldc_I4_0]; Locals = [] })
            | _ -> Success({ Instructions = [Ldc_I4 n]; Locals = [] })
        | Binary (lhs, oper, rhs) -> 
            let lhsMethod = codegenExpr lhs
            let rhsMethod = codegenExpr rhs
            let operInst = codegen_oper oper
            match (lhsMethod, rhsMethod) with
                | (Success l, Success r) -> Success({ Instructions = List.concat [ l.Instructions; r.Instructions; [operInst] ]; Locals = List.concat [l.Locals; r.Locals] })
                | (Error l, _) -> lhsMethod
                | (_, Error r) -> rhsMethod

    let rec codegen : Try<Expr list, string> -> Try<Method, string> = function
        | Success expList -> codegenExpr expList.Head
        | Error(e) -> Error(e)
