module Generator


open System.IO
open System
open writePrint


type DataType =
    | Int
    | Bool
    | Float


type GeneratorConfig =
    val dataType: DataType
    val lines: int
    val cols: int
    val sparsity: float // shows percentage of neutral elements
    val amount: int
    val path: string
    new (a, b, c, d, e, f) = {dataType = a; lines = b; cols = c; sparsity = d; amount = e; path = f}


let generateMatrix (config: GeneratorConfig) =
    let rand = Random()
    for number in 0 .. config.amount - 1 do
        let output = Array2D.zeroCreate config.lines config.cols
        for i in 0 .. config.lines - 1 do
            for j in 0 .. config.cols - 1 do
                let randNum = rand.NextDouble()
                if randNum > config.sparsity
                then
                    output.[i, j] <- match config.dataType with
                                      | Int -> rand.Next() + 1 |> string // + 1 in that case, when rand.Next() = 0
                                      | Float -> rand.Next() + 1 |> float |> string
                                      | Bool -> "1"
                else output.[i, j] <- "0"
        printMatrix output <| Path.Combine(config.path, "Matrix" + string number + ".txt")
