namespace homework


open System
open QuadTree
open Output

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        printf "enter path to file: "
        let pathIn = Console.ReadLine()
        let sparse = toBoolSparse <| readToSparseMatrix pathIn
        let exTree = extendedTree.createTreeOfSparseMatrix sparse
        printf "enter path of out: "
        let pathOut = Console.ReadLine()
        extTreeToDotAfterTrClosure pathOut exTree
        0
