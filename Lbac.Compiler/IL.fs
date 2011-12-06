module IL
    open System
    open System.Reflection
    open System.Reflection.Emit

    type instruction = 
        | Add 
        | Ldc_I4    of int
        | Ldc_I4_0
        | Ret
        | Sub

    let emit (ilg : Emit.ILGenerator) inst = 
        match inst with 
        | Add        -> ilg.Emit(OpCodes.Add)
        | Ldc_I4   n -> ilg.Emit(OpCodes.Ldc_I4, n)
        | Ldc_I4_0   -> ilg.Emit(OpCodes.Ldc_I4_0)
        | Ret        -> ilg.Emit(OpCodes.Ret)
        | Sub        -> ilg.Emit(OpCodes.Sub)

    let compileMethod(instructions: seq<instruction>) =
        let moduleName = "test" 
        let className = "CompiledCode"
        let entryPoint = "Main"
        let an = new AssemblyName(moduleName)
        let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.Run)
        let modb = ab.DefineDynamicModule moduleName 
        let tb = modb.DefineType className
        let mb = tb.DefineMethod(entryPoint, MethodAttributes.Static, null, Type.EmptyTypes)
        let ilg = mb.GetILGenerator()
        let ilEmit = emit ilg
        for instruction in instructions do
            ilEmit instruction
        ilEmit Ret
        ignore tb.CreateType
        modb.CreateGlobalFunctions()
        ab.SetEntryPoint mb
        ab.EntryPoint


