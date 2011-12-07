﻿module IL
    open System
    open System.Reflection
    open System.Reflection.Emit

    type instruction = 
        | Add 
        | Div
        | Ldc_I4    of int
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
        | Add        -> ilg.Emit(OpCodes.Add)
        | Div        -> ilg.Emit(OpCodes.Div)
        | Ldc_I4   n -> ilg.Emit(OpCodes.Ldc_I4, n)
        | Ldc_I4_0   -> ilg.Emit(OpCodes.Ldc_I4_0)
        | Ldloc_0    -> ilg.Emit(OpCodes.Ldloc_0)
        | Ldloc_1    -> ilg.Emit(OpCodes.Ldloc_1)
        | Mul        -> ilg.Emit(OpCodes.Mul)
        | Ret        -> ilg.Emit(OpCodes.Ret)
        | Stloc_0    -> ilg.Emit(OpCodes.Stloc_0)
        | Stloc_1    -> ilg.Emit(OpCodes.Stloc_1)
        | Sub        -> ilg.Emit(OpCodes.Sub)

    let compileMethod(instructions: seq<instruction>) =
        let moduleName = "test" 
        let className = "CompiledCode"
        let entryPoint = "Main"
        let an = new AssemblyName(moduleName)
        let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.Run)
        let modb = ab.DefineDynamicModule moduleName 
        let tb = modb.DefineType(className, TypeAttributes.Public)
        let mb = tb.DefineMethod(entryPoint, MethodAttributes.Public, null, null)
        let ilg = mb.GetILGenerator() |> emit
        for instruction in instructions do
            ilg instruction
        ilg Ret
        let t = tb.CreateType()
        modb.CreateGlobalFunctions()
        (t, mb)

    let execute instructions =
        let (t, m) = compileMethod instructions
        let instance = Activator.CreateInstance(t)
        t.GetMethod("Main").Invoke(instance, null)

    let print (instructions: seq<instruction>) =
        let p = sprintf "%A"
        Seq.map p instructions
        
