module IL
    open System
    open System.Reflection
    open System.Reflection.Emit

    type instruction = 
        | Add 
        | DeclareLocal of System.Type
        | Div
        | Ldc_I4       of int
        | Ldc_I4_0
        | Ldloc_0
        | Ldloc_1
        | Mul
        | Ret
        | Stloc_0
        | Stloc_1
        | Sub

    let emit (ilg : Emit.ILGenerator) inst = 
        match inst with 
        | Add            -> ilg.Emit(OpCodes.Add)
        | DeclareLocal t -> ignore(ilg.DeclareLocal(t))
        | Div            -> ilg.Emit(OpCodes.Div)
        | Ldc_I4   n     -> ilg.Emit(OpCodes.Ldc_I4, n)
        | Ldc_I4_0       -> ilg.Emit(OpCodes.Ldc_I4_0)
        | Ldloc_0        -> ilg.Emit(OpCodes.Ldloc_0)
        | Ldloc_1        -> ilg.Emit(OpCodes.Ldloc_1)
        | Mul            -> ilg.Emit(OpCodes.Mul)
        | Ret            -> ilg.Emit(OpCodes.Ret)
        | Stloc_0        -> ilg.Emit(OpCodes.Stloc_0)
        | Stloc_1        -> ilg.Emit(OpCodes.Stloc_1)
        | Sub            -> ilg.Emit(OpCodes.Sub)

    let compileMethod(instructions: seq<instruction>) (methodResultType) =
        let assemName = "test.exe" 
        let className = "CompiledCode"
        let entryPoint = "Main"
        let moduleName = "TestModule"
        let an = new AssemblyName(assemName)
        let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.RunAndSave)
        let modb = ab.DefineDynamicModule(moduleName, assemName)
        let ta = TypeAttributes.Public ||| TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
        let tb = modb.DefineType(className, ta)
        let ma = MethodAttributes.Public ||| MethodAttributes.HideBySig
        let mb = tb.DefineMethod(entryPoint, ma, methodResultType, System.Type.EmptyTypes)
        let ilg = mb.GetILGenerator() |> emit
        let declare = DeclareLocal typeof<int>
        ilg declare
        for instruction in instructions do
            ilg instruction
        ilg Ret
        
        let t = tb.CreateType()
        modb.CreateGlobalFunctions()
        ab.Save(moduleName)
        (t, mb)

    let execute<'TMethodResultType> instructions =
        let (t, m) = compileMethod instructions typeof<'TMethodResultType>
        let instance = Activator.CreateInstance(t)
        t.GetMethod("Main").Invoke(instance, null) :?> 'TMethodResultType

    let print (instructions: seq<instruction>) =
        let p = sprintf "%A"
        Seq.map p instructions
        
