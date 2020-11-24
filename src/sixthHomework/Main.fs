namespace homework

module Main =
    open System

    [<EntryPoint>]
    let main (argv: string array) =
        printfn "enter path of first matrix: "
        let firstMatrix = myMatrix.readBoolMatrix <| Console.ReadLine()
        printfn "enter path of second matrix: "
        let sndMatrix = myMatrix.readBoolMatrix <| Console.ReadLine()
        let result = myMatrix.multiplyingBoolMatrix firstMatrix sndMatrix
        printfn "enter path of output file: "
        myMatrix.outBoolMatrix result <| Console.ReadLine()
        0
