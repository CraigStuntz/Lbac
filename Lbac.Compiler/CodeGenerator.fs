module CodeGenerator

    open Railway
    open IL
    open Syntax

    let private localVarIndex locals name = 
        List.tryFindIndex (fun l -> System.String.Equals(l, name, System.StringComparison.Ordinal)) locals

    let private codegenAssign locals name =
        match localVarIndex locals name with
            | None   -> Failure ("Undeclared variable " + name)
            | Some i -> Success(Stloc i)

    let private codegenOper = function
        | Add      -> instruction.Add
        | Subtract -> instruction.Sub
        | Multiply -> instruction.Mul
        | Divide   -> instruction.Div

    let private tryLdLoc ((locals : string list), (name : string)) = 
        match localVarIndex locals name with
            | None   -> Failure ("Undeclared variable " + name)
            | Some i -> Success(Ldloc i)

    let rec codegenExpr (acc : Method) (expr : Expr) = 
        match expr with
        | Variable v -> 
            match tryLdLoc (acc.Locals, v) with 
            | Success inst -> Success({ acc with Instructions = acc.Instructions @ [inst] })
            | Failure   err  -> Failure err
        | Invoke m -> Failure "Sorry; no can do"
        | Minus e -> 
            match codegenExpr acc e with
            | Success m -> Success({ m with Instructions = m.Instructions @ [Neg] })
            | err -> err
        | Number n -> Success({ acc with Instructions = acc.Instructions @ [Ldc_I4 n] })
        | Assign (n, rhs) -> 
            let rhsMethod = codegenExpr { acc with Instructions = [] } rhs
            match (n, rhsMethod) with 
            | (Variable name, Success r) -> 
                match codegenAssign r.Locals name with 
                | Success assign_inst -> 
                    let insts = List.concat [ r.Instructions; [ assign_inst ] ]
                    Success({ Instructions = acc.Instructions @ insts; Locals = r.Locals })
                | Failure a -> Failure(a)
            | (_, Success r) -> failwith "A variable is required on the left hand side of an assignment." // Should never happen; parser should not emit this
            | (_, Failure r) -> rhsMethod
        | Binary (lhs, oper, rhs) -> 
            let lhsMethod = codegenExpr { acc with Instructions = [] } lhs
            let rhsMethod = codegenExpr { acc with Instructions = [] } rhs
            let operInst = codegenOper oper
            match (lhsMethod, rhsMethod) with
                | (Success l, Success r) -> 
                    let insts       = List.concat [ l.Instructions; r.Instructions; [operInst] ]
                    let mergeLocals = List.concat [ l.Locals; List.filter (fun i2 -> not (List.exists (fun i1 -> i1 = i2) l.Locals)) r.Locals]
                    Success({ Instructions = acc.Instructions @ insts; Locals = mergeLocals })
                | (Failure l, _) -> lhsMethod
                | (_, Failure r) -> rhsMethod
        | Error _ -> failwith "Sorry, you can't pass an Error here!"

    let rec codegen (parsed : ParseResult) =
        let locals = 
            parsed.Locals 
            |> List.ofSeq
        let tryCodeGenLine acc line = 
            match acc, line with
            | Success accMethod, expr -> codegenExpr accMethod expr
            | Failure err, _ -> Failure err
        let localDeclarations = [for name in locals -> DeclareLocal(typedefof<int>)]
        let emptyMethod = Success( { Instructions = localDeclarations; Locals = locals } )
        List.fold tryCodeGenLine emptyMethod parsed.Lines
