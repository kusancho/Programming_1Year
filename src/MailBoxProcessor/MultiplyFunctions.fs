module MultiplyFunctions


open AlgebraicStructure
open QuadTree
open homework.matrixes


let defaultSemiRing = SemiRing(new SemiRing<_>(new Monoid<_>((+), 0), (*)))


let quadTreeHelper (arr: int [][]) =
    extendedTree.init (arr.GetLength 0) (arr.GetLength 1) (fun i j -> arr.[i].[j])


let quadTreeMultiply (a: extendedTree<_>) b = a.multiply b defaultSemiRing


let quadTreeParallelMultiply depth (a: extendedTree<_>) b = a.parallelMultiply b defaultSemiRing depth


let arrMultiply a b = multiplyMartix a b


let arrMultiplyParallel n (a: _ [][]) (b: _ [][]) =
    let res = Array2D.zeroCreate (a.GetLength 0) (b.GetLength 1)
    let chunkSize = (a.GetLength 0 - 1) / n
    [ for p in 0 .. n - 1 ->
          async { do
                      for i in p * chunkSize ..  chunkSize * (p + 1) - 1 do
                          for j in 0 .. b.GetLength 1 - 1 do
                              for k in 0 .. a.GetLength 1 - 1 do
                                  res.[i, j] <- res.[i, j] + (a.[i].[k] * b.[k].[j])
          }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    res
