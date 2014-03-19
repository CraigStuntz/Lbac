module OptimizeIL

    open IL
    open System

    let private maxByte = Convert.ToInt32 System.Byte.MaxValue

    let private optimalShortEncodingFor = function
        | Ldc_I4 0    -> Ldc_I4_0 
        | Ldc_I4 1    -> Ldc_I4_1 
        | Ldc_I4 2    -> Ldc_I4_2 
        | Ldc_I4 3    -> Ldc_I4_3 
        | Ldc_I4 4    -> Ldc_I4_4 
        | Ldc_I4 5    -> Ldc_I4_5 
        | Ldc_I4 6    -> Ldc_I4_6 
        | Ldc_I4 7    -> Ldc_I4_7 
        | Ldc_I4 8    -> Ldc_I4_8 
        | Ldloc 0     -> Ldloc_0  
        | Ldloc 1     -> Ldloc_1  
        | Ldloc 2     -> Ldloc_2  
        | Ldloc 3     -> Ldloc_3
        | Ldloc i when i <= maxByte -> Ldloc_S(Convert.ToByte(i))
        | Stloc 0     -> Stloc_0  
        | Stloc 1     -> Stloc_1  
        | Stloc 2     -> Stloc_2  
        | Stloc 3     -> Stloc_3  
        | Stloc i when i <= maxByte -> Stloc_S(Convert.ToByte(i))
        | instruction -> instruction

    let optimize (m : Method) = { m with Instructions = List.map optimalShortEncodingFor m.Instructions }