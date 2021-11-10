namespace homework

open QuadTree
open AlgebraicStructure

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let semiRing = SemiRing (new SemiRing<int>(new Monoid<int>((+), 0), (*)))
        let multSeq (fst: extendedTree<_>) snd = fst.multiply snd semiRing
        let sparsity = 0.8
        let multParallel depth (fst: extendedTree<_>) snd = fst.parallelMultiply snd semiRing depth
        Comparison.createCSV (__SOURCE_DIRECTORY__ + $"/Plots/seq{sparsity}.csv") (__SOURCE_DIRECTORY__ + $"/Plots/parallel{sparsity}.csv")
                   multSeq (multParallel 1) 7000 45000 1000 3 sparsity
        0
