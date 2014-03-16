module OptimizeIL

    open IL

    let rec optimalShortEncodingFor = function
        | Ldc_I4  0   :: rest   -> Ldc_I4_0 :: optimalShortEncodingFor rest
        | Ldc_I4  1   :: rest   -> Ldc_I4_1 :: optimalShortEncodingFor rest
        | Ldc_I4  2   :: rest   -> Ldc_I4_2 :: optimalShortEncodingFor rest
        | Ldc_I4  3   :: rest   -> Ldc_I4_3 :: optimalShortEncodingFor rest
        | Ldc_I4  4   :: rest   -> Ldc_I4_4 :: optimalShortEncodingFor rest
        | Ldc_I4  5   :: rest   -> Ldc_I4_5 :: optimalShortEncodingFor rest
        | Ldc_I4  6   :: rest   -> Ldc_I4_6 :: optimalShortEncodingFor rest
        | Ldc_I4  7   :: rest   -> Ldc_I4_7 :: optimalShortEncodingFor rest
        | Ldc_I4  8   :: rest   -> Ldc_I4_8 :: optimalShortEncodingFor rest
        | Ldloc_S 0uy :: rest   -> Ldloc_0  :: optimalShortEncodingFor rest
        | Ldloc_S 1uy :: rest   -> Ldloc_1  :: optimalShortEncodingFor rest
        | Ldloc_S 2uy :: rest   -> Ldloc_2  :: optimalShortEncodingFor rest
        | Ldloc_S 3uy :: rest   -> Ldloc_3  :: optimalShortEncodingFor rest
        | Stloc_S 0uy :: rest   -> Stloc_0  :: optimalShortEncodingFor rest
        | Stloc_S 1uy :: rest   -> Stloc_1  :: optimalShortEncodingFor rest
        | Stloc_S 2uy :: rest   -> Stloc_2  :: optimalShortEncodingFor rest
        | Stloc_S 3uy :: rest   -> Stloc_3  :: optimalShortEncodingFor rest
        | instruction :: rest   -> instruction :: optimalShortEncodingFor rest
        | [] -> [] 

    let optimize (m : Method) = { m with Instructions = optimalShortEncodingFor m.Instructions }