module Config


type Method =
    | QtParallel
    | QtDefault
    | ArrParallel
    | ArrDefault


type TypeProcessing =
    | All
    | Amount of int


type Config =
    val Amount: TypeProcessing
    val InPath: string
    val OutPath: string
    val QuadTreeParallelDepth: int
    val ArrayParallelDepth: int
    val MethodMap: float * float -> Method


    new (amount, inPath, outPath, qtDepth, arrDepth, map) =
        {Amount = amount
         InPath = inPath
         OutPath = outPath
         QuadTreeParallelDepth = qtDepth
         ArrayParallelDepth = arrDepth
         MethodMap = map}


    static member makeDefaultMethodMap border (i, j) =
        match (i, j) with
            | i, j when i < border && j < border -> QtDefault
            | i, j when i < border && j >=  border -> QtDefault
            | i, j when i > border && j >=  border -> QtDefault
            | i, j when i > border && j <  border -> QtDefault
            | _, _ -> QtDefault
