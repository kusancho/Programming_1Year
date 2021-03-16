namespace homework


open System
open AlgebraicStructure
open LongInt
open MyList
open QuadTree
open Output

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        //printf "enter path to file: "
        //let pathIn = Console.ReadLine()
        let pathIn: string = __SOURCE_DIRECTORY__ + "/input.txt"
        let pathOut: string  = __SOURCE_DIRECTORY__ + "/output.dot"
        let sparse = readBoolMatrix pathIn
        let exTree = extendedTree.createTreeOfSparseMatrix sparse
        //printf "enter path of out: "
        //let pathOut = Console.ReadLine()
        extTreeToDotAfterTrClosure pathOut exTree
        0
