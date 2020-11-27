namespace homework

open myMatrix

module Main =
    open System

    [<EntryPoint>]
    let main (argv: string array) =
        (*printfn "enter path of first matrix: "
        let firstMatrix = myMatrix.readBoolMatrix <| Console.ReadLine()
        printfn "enter path of second matrix: "
        let sndMatrix = myMatrix.readBoolMatrix <| Console.ReadLine()
        let result = myMatrix.multiplyingBoolMatrix firstMatrix sndMatrix
        printfn "enter path of output file: "
        myMatrix.outBoolMatrix result <| Console.ReadLine()*)
        printfn "enter path: "
        let path = Console.ReadLine()
        outBoolMatrix (BoolMatrix(5,5,[Coordinates(4<line>, 1<col>); Coordinates(2<line>, 2<col>)])) path
        0
