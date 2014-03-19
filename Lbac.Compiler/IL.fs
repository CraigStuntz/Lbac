module IL
    open System
    open System.Reflection
    open System.Reflection.Emit

    type instruction = 
        | Add 
        | Call         of System.Reflection.MethodInfo
        | Callvirt     of System.Reflection.MethodInfo
        | DeclareLocal of System.Type
        | Div
        | Ldc_I4       of int
        | Ldc_I4_0
        | Ldc_I4_1
        | Ldc_I4_2
        | Ldc_I4_3
        | Ldc_I4_4
        | Ldc_I4_5
        | Ldc_I4_6
        | Ldc_I4_7
        | Ldc_I4_8
        | Ldloc        of int
        | Ldloc_0
        | Ldloc_1
        | Ldloc_2
        | Ldloc_3
        | Ldloc_S      of byte
        | Mul
        | Neg
        | Newobj       of System.Reflection.ConstructorInfo
        | Nop
        | Pop
        | Refanyval
        | Ret
        | Stloc        of int
        | Stloc_0
        | Stloc_1
        | Stloc_2
        | Stloc_3
        | Stloc_S      of byte
        | Sub

    type Method = { Instructions: instruction list; Locals: string list } 
     
    let private emit (ilg : Emit.ILGenerator) inst = 
        match inst with 
        | Add            -> ilg.Emit(OpCodes.Add)
        | Call mi        -> ilg.Emit(OpCodes.Call, mi)
        | Callvirt mi    -> ilg.Emit(OpCodes.Callvirt, mi)
        | DeclareLocal t -> ignore(ilg.DeclareLocal(t))
        | Div            -> ilg.Emit(OpCodes.Div)
        | Ldc_I4 n       -> ilg.Emit(OpCodes.Ldc_I4, n)
        | Ldc_I4_0       -> ilg.Emit(OpCodes.Ldc_I4_0)
        | Ldc_I4_1       -> ilg.Emit(OpCodes.Ldc_I4_1)
        | Ldc_I4_2       -> ilg.Emit(OpCodes.Ldc_I4_2)
        | Ldc_I4_3       -> ilg.Emit(OpCodes.Ldc_I4_3)
        | Ldc_I4_4       -> ilg.Emit(OpCodes.Ldc_I4_4)
        | Ldc_I4_5       -> ilg.Emit(OpCodes.Ldc_I4_5)
        | Ldc_I4_6       -> ilg.Emit(OpCodes.Ldc_I4_6)
        | Ldc_I4_7       -> ilg.Emit(OpCodes.Ldc_I4_7)
        | Ldc_I4_8       -> ilg.Emit(OpCodes.Ldc_I4_8)
        | Ldloc    i     -> ilg.Emit(OpCodes.Ldloc, i)
        | Ldloc_0        -> ilg.Emit(OpCodes.Ldloc_0)
        | Ldloc_1        -> ilg.Emit(OpCodes.Ldloc_1)
        | Ldloc_2        -> ilg.Emit(OpCodes.Ldloc_2)
        | Ldloc_3        -> ilg.Emit(OpCodes.Ldloc_3)
        | Ldloc_S  i     -> ilg.Emit(OpCodes.Ldloc_S, i)
        | Mul            -> ilg.Emit(OpCodes.Mul)
        | Neg            -> ilg.Emit(OpCodes.Neg)
        | Newobj   ci    -> ilg.Emit(OpCodes.Newobj, ci)
        | Nop            -> ilg.Emit(OpCodes.Nop)
        | Pop            -> ilg.Emit(OpCodes.Pop)
        | Refanyval      -> ilg.Emit(OpCodes.Refanyval)
        | Ret            -> ilg.Emit(OpCodes.Ret)
        | Stloc    i     -> ilg.Emit(OpCodes.Stloc, i)
        | Stloc_0        -> ilg.Emit(OpCodes.Stloc_0)
        | Stloc_1        -> ilg.Emit(OpCodes.Stloc_1)
        | Stloc_2        -> ilg.Emit(OpCodes.Stloc_2)
        | Stloc_3        -> ilg.Emit(OpCodes.Stloc_3)
        | Stloc_S  i     -> ilg.Emit(OpCodes.Stloc_S, i)
        | Sub            -> ilg.Emit(OpCodes.Sub)

    let private compileEntryPoint (moduleContainingMethod : ModuleBuilder) (methodToCall: MethodBuilder) = 
        let mb = 
            let tb = 
                let className = "Program"
                let ta = TypeAttributes.NotPublic ||| TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
                moduleContainingMethod.DefineType(className, ta)
            let ma = MethodAttributes.Public ||| MethodAttributes.Static 
            let methodName = "Main"
            tb.DefineMethod(methodName, ma)
        let ilg = mb.GetILGenerator() |> emit
        let ci = methodToCall.ReflectedType.GetConstructor([||])
        ilg (Newobj ci)
        ilg (Call methodToCall)
        if methodToCall.ReturnType <> null then
            ilg (DeclareLocal methodToCall.ReturnType)
            ilg Stloc_0
            ilg (Ldloc_S 0uy)
            let mi = methodToCall.ReturnType.GetMethod("ToString", [||])
            ilg (Call mi)
            let writeln = typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.String> |])
            ilg (Call writeln)
        ilg Ret
        mb

    let private entryPointMethodName = "__Main"

    let compileMethod(moduleName: string) (instructions: seq<instruction>) (methodResultType) =
        let assemblyBuilder = 
            let assemblyName = AssemblyName(moduleName)
            AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(moduleName)
        let typeBuilder = 
            let className = "__CompiledMethods"
            let typeAttributes = TypeAttributes.Public ||| TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
            moduleBuilder.DefineType(className, typeAttributes)
        let methodBuilder = 
            let methodAttributes = MethodAttributes.Public ||| MethodAttributes.HideBySig
            typeBuilder.DefineMethod(entryPointMethodName, methodAttributes, methodResultType, System.Type.EmptyTypes)
        let ilg = methodBuilder.GetILGenerator() |> emit
        Seq.iter ilg instructions 
        ilg Ret
        
        let entryPointType = typeBuilder.CreateType()
        let entryPoint = compileEntryPoint moduleBuilder methodBuilder
        assemblyBuilder.SetEntryPoint(entryPoint, PEFileKinds.ConsoleApplication)
        moduleBuilder.CreateGlobalFunctions()
        (entryPointType, assemblyBuilder)

    let toMethod resultType methodWithInstructions =
        let moduleName = "test.exe"
        let (t, _) = compileMethod moduleName methodWithInstructions.Instructions resultType
        t.GetMethod(entryPointMethodName)

    let execute<'TMethodResultType> (instructions, saveAs) =
        let moduleName = match saveAs with
                         | Some s -> s
                         | None   -> "test.exe"
        let (entryPointType, assemblyBuilder) = compileMethod moduleName instructions typeof<'TMethodResultType>
        if saveAs.IsSome then 
            assemblyBuilder.Save(entryPointType.Module.ScopeName)
        let instance = Activator.CreateInstance(entryPointType)
        entryPointType.GetMethod(entryPointMethodName).Invoke(instance, null) :?> 'TMethodResultType

    let print (instructions: seq<instruction>) =
        let p = sprintf "%A"
        Seq.map p instructions        