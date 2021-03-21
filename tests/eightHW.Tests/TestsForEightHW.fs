module TestsForEighthHW

open Expecto
open QuadTree
open AlgebraicStructure
open SparseMatrix


let genArrayBySparseMatrix (mtx: SparseMatrix<int>) =
    let output = Array2D.zeroCreate mtx.lineSize mtx.colSize
    for x in mtx.content do
        output.[x.line, x.col] <- x.data
    output


let arrSum (x: int[,]) (y: int[,]) =
    let a = Array2D.copy x
    if Array2D.length1 x <> Array2D.length1 y || Array2D.length2 x <> Array2D.length2 y
    then failwith "Dimensions of matrices should be equal"
    else
        for i in 0 .. Array2D.length1 x - 1 do
            for j in 0 .. Array2D.length2 x - 1 do
                a.[i, j] <- a.[i, j] + y.[i, j]
    a


let multiplyByScalar s (x:int[,]) =
    let y = Array2D.copy x
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            y.[i, j] <- s * y.[i, j]
    y

let tensor (x1: SparseMatrix<int>) (y1: SparseMatrix<int>) =
    let x = genArrayBySparseMatrix x1
    let y = genArrayBySparseMatrix y1
    let rowsX, rowsY = x.[0, *].Length, y.[0, *].Length
    let colsX, colsY = x.[*, 0].Length, y.[*, 0].Length
    let rows, cols = rowsX * rowsY, colsX * colsY
    let res = Array2D.zeroCreate rows cols
    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            res.[i, j] <- x.[i / rowsY, j / colsY] * y.[i % rowsY, j % rowsY]
    res


let matrixMultiply (matrix1: int[,]) (matrix2: int[,]) =
    let row1 = matrix1.GetLength 0
    let col1 = matrix1.GetLength 1
    let col2 = matrix2.GetLength 1
    let result = Array2D.zeroCreate row1 col2
    if row1 = col2
    then
        for i in 0 .. row1 - 1 do
            for k in 0 .. col2 - 1 do
                for r in 0 .. col1 - 1 do
                        result.[i,k] <- result.[i,k] + matrix1.[i,r] * matrix2.[r,k]
        result
    else failwith "wrong matrices"


//experimental trees
let fst = Node(Leaf(-7),  Leaf(-10),   Leaf(10),    Leaf(-10))        // TREES FOR SUM
let snd = Node(Leaf(7),   Leaf(10),    Leaf(-10),   Leaf(10))
let sum = None
let forReduce = Node(None, None, None, Leaf 1)
let errorTree = Leaf(404)
let monoid = Monoid(new Monoid<int>((+), 0))
let semiRing = new SemiRing<int>(new Monoid<int>((+), 0), (*))
let semiRingAlg = SemiRing (new SemiRing<int>(new Monoid<int>((+), 0), (*)))
let fstMultTree = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(1, 5, [Cell(0, 0, 100)]))       // trees for mupliply
let sndMultTree = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(5, 2, [Cell(0, 0, 3); Cell(0, 1, 3)]))
let resMultTree = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(1, 2, [Cell(0, 0, 300); Cell(0, 1, 300)]))


[<Tests>]
let testTree =
    testList "Trees functions" [

        testProperty "autoTests toTree/ofTree" <| fun (x: int) ->
            let size = (abs x) % 50 + 2
            let sparse = randomIntSparseMatrix (size + 10) size
            let tree = extendedTree.createTreeOfSparseMatrix monoid sparse
            let arr = genArrayBySparseMatrix sparse
            Expect.equal (genArrayBySparseMatrix <| extendedTree.toSparseMatrix tree) arr " "


        testProperty "autoTests tensorMultiply" <| fun (x1: int) (x2: int)  ->
            let fst = getPowOfTwo <| abs(x1 % 20) + 5
            let snd = getPowOfTwo <| abs(x2 % 30) + 5
            let mtx1 = randomIntSparseMatrix (fst) (fst)
            let mtx2 = randomIntSparseMatrix snd  snd
            Expect.equal
                (tensor mtx1 mtx2)
                (genArrayBySparseMatrix
                (extendedTree.toSparseMatrix
                (extendedTree.tensorMultiply (extendedTree.createTreeOfSparseMatrix semiRingAlg mtx1) (extendedTree.createTreeOfSparseMatrix semiRingAlg mtx2) semiRingAlg))) ""


        testProperty "autoTests sum" <| fun (x: int) ->
            let size = (abs x) % 50 + 2
            let fSparse, sSparse = (randomIntSparseMatrix size size), (randomIntSparseMatrix size size)
            let fTree, sTree = (extendedTree.createTreeOfSparseMatrix monoid fSparse), (extendedTree.createTreeOfSparseMatrix monoid sSparse)
            let fArr, sArr = (genArrayBySparseMatrix fSparse), (genArrayBySparseMatrix sSparse)
            let sumArr = arrSum fArr sArr
            let sparseSum = extendedTree.toSparseMatrix <| extendedTree.sumExTree fTree sTree monoid
            Expect.equal (genArrayBySparseMatrix sparseSum) sumArr


        testProperty "autoTests multiply" <| fun (x: int) (y: int) ->
            let size1 = (abs x) % 50 + 2
            let size2 = (abs y) % 20 + 2
            let fSparse, sSparse = (randomIntSparseMatrix size1 size2), (randomIntSparseMatrix size2 size1)
            let fTree, sTree = (extendedTree.createTreeOfSparseMatrix monoid fSparse), (extendedTree.createTreeOfSparseMatrix monoid sSparse)
            let fArr, sArr = (genArrayBySparseMatrix fSparse), (genArrayBySparseMatrix sSparse)
            let multArr = matrixMultiply fArr sArr
            let sparseMult = extendedTree.toSparseMatrix <| extendedTree.multiply fTree sTree semiRingAlg
            Expect.equal (genArrayBySparseMatrix sparseMult) multArr


        testCase "extreme case #1" <| fun _ ->
            let mtx = SparseMatrix(1, 5, [])
            Expect.equal (extendedTree.createTreeOfSparseMatrix monoid mtx).tree None ""


        testCase "extreme case #2" <| fun _ ->
            let mtx = SparseMatrix(1, 1, [Cell(0, 0, 0)])
            Expect.equal (extendedTree.createTreeOfSparseMatrix monoid mtx).tree None ""


        testCase "sum of Trees" <| fun _ ->
            Expect.equal (quadTree.sum fst snd monoid) sum ""


        testCase "sum of Trees #2" <| fun _ ->
            Expect.throws (fun _ -> quadTree.sum fst errorTree monoid |> ignore) ""


        testCase "reduce" <| fun _ ->
            Expect.equal (quadTree.reduce forReduce 1 2) (Leaf 1) ""


        testCase "scalarMultiply #1" <| fun _ ->
            Expect.equal (quadTree.scalarMultiply fst 0 semiRing.Mul semiRing.Monoid.Neutral) None ""


        testCase "scalarMultiply #2" <| fun _ ->
            Expect.equal (quadTree.scalarMultiply fst -2 semiRing.Mul semiRing.Monoid.Neutral) (Node(Leaf(14),  Leaf(20),      Leaf(-20),  Leaf(20)))  ""


        testCase "align" <| fun _ ->
            let fst, snd = extendedTree.alignTrees (extendedTree(1, 1, errorTree)) (extendedTree(2, 2, sum)) monoid
            Expect.equal fst.tree (Node(Leaf 404, None, None, None)) ""


        testCase "multiply" <| fun _ ->
            Expect.equal (extendedTree.multiply fstMultTree sndMultTree (SemiRing(semiRing))) resMultTree ""


        testCase "tensorMultiply" <| fun _ ->
            let fst = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(1, 1, [Cell(0, 0, 1)]))
            let snd = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(2, 2, [Cell(0, 0, 10); Cell(1, 0, 10)]))
            let res = extendedTree.createTreeOfSparseMatrix monoid (SparseMatrix(2, 2, [Cell(0, 0, 10); Cell(1, 0, 10)]))
            Expect.equal (extendedTree.tensorMultiply fst snd (SemiRing(semiRing))) res ""
             ]


let mtx = SparseMatrix(3, 2, [Cell(0,0, 100); Cell(2, 1, 101)])


[<Tests>]
let testSparseMatrix =
    testList "SparseMatrix functions" [
            testCase "getIntContent" <| fun _ ->
                Expect.equal (SparseMatrix.getContent mtx 0 0) 100 ""


            testCase "getIntContent throw" <| fun _ ->
                Expect.throws (fun _ -> SparseMatrix.getContent mtx 1 0 |> ignore) ""


            testCase "toLisOfCells" <| fun _ ->
                Expect.equal (SparseMatrix.toListOfCells mtx) [(0, 0, 100); (2, 1, 101)] ""


            testCase "isEmptyOnPlace" <| fun _ ->
                Expect.equal (SparseMatrix.isEmptyInPlace mtx (0, 0) (0, 0)) false ""


            testCase "isEmptyOnPlace#2" <| fun _ ->
                Expect.equal (SparseMatrix.isEmptyInPlace mtx (1, 1) (1, 1)) true ""  ]
