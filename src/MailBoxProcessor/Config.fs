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
    val Path: string
    val QuadTreeParallelDepth: int
    val ArrayParallelDepth: int
    val MethodMap: float * float -> Method


    new (amount, path, qtDepth, arrDepth, map) =
        {Amount = amount; Path = path; QuadTreeParallelDepth = qtDepth; ArrayParallelDepth = arrDepth; MethodMap = map}


    static member makeDefaultMethodMap lower medium (i, j) =
        match (i, j) with

            | i, j when i < lower && j < lower -> QtDefault

            | i, j when i < lower && j >  lower -> QtParallel

            | i, j when i > medium && j >  medium -> ArrParallel

            | i, j when i > lower && j <  medium -> ArrDefault

            | _, _ -> ArrParallel
