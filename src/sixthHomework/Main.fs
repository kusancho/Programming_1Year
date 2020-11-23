namespace homework

module Main =
    open System

    [<EntryPoint>]
    let main (argv: string array) =
        printfn "enter path of first matrix: "
        let firstMatrix = myMatrix.readMyMatrix <| Console.ReadLine()
        printfn "enter path of second matrix: "
        let sndMatrix = myMatrix.readMyMatrix <| Console.ReadLine()
        let result = myMatrix.multiplyingSparseMatrix firstMatrix sndMatrix
        printfn "enter path of output file: "
        myMatrix.outSparseMatrix result <| Console.ReadLine()
        0
