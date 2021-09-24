namespace homework


open System
open QuadTree
open Output
open AlgebraicStructure


module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let monoid = Monoid(new Monoid<int>((+), 0))
        let semiRing = SemiRing(new SemiRing<_>(new Monoid<int>((+), 0), (*)))
        printf "enter path to file: "
        let pathIn = Console.ReadLine()
        let sparse = toIntSparse <| readToSparseMatrix pathIn
        let exTree = extendedTree.createTreeOfSparseMatrix monoid sparse
        printf "enter path of out: "
        let pathOut = Console.ReadLine()
        extTreeToDotAfterTrClosure pathOut exTree semiRing
        0
