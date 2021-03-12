module TestsForEighthHW

open Expecto
open QuadTree
open AlgebraicStructure


//experimental trees
let fst = Node(Leaf(3),  None,      Leaf(10),  None)        // TREES FOR SUM
let snd = Node(Leaf(7),  Leaf(10),  None,      Leaf(10))
let sum = Node(Leaf(10), Leaf(10),  Leaf(10),  Leaf(10))
let forReduce = Node(None, None, None, Leaf 1)
let errorTree = Leaf(404)
let monoid = Monoid(new Monoid<int>((+), 0))
let semiRing = new SemiRing<int>(new Monoid<int>((+), 0), (*))
let fstMultTree = extendedTree.createTreeOfSparseMatrix (SparseMatrix(1, 5, [Cell(0, 0, 1)]))       // trees for mupliply
let sndMultTree = extendedTree.createTreeOfSparseMatrix (SparseMatrix(5, 2, [Cell(0, 0, 3); Cell(0, 1, 3)]))
let resMultTree = extendedTree.createTreeOfSparseMatrix (SparseMatrix(1, 2, [Cell(0, 0, 3); Cell(0, 1, 3)]))


[<Tests>]
let testTree =
    testList "Trees functions" [
        testProperty "autoTests toTree/ofTree" <| fun x ->
            let size = (abs x) % 50 + 2
            let sparse = randomIntSparseMatrix size
            let tree = extendedTree.createTreeOfSparseMatrix sparse
            let func (a, b, _) = a, b
            Expect.equal (List.sortBy func (SparseMatrix.toListOfCells sparse)) (List.sortBy func (SparseMatrix.toListOfCells <| extendedTree.toSparseMatrix tree))


        testCase "extreme case #1" <| fun _ ->
            let mtx = SparseMatrix(0, 0, [])
            Expect.equal (extendedTree.createTreeOfSparseMatrix mtx).tree None ""


        testCase "extreme case #2" <| fun _ ->
            let mtx = SparseMatrix(1, 1, [Cell(0, 0, 3)])
            Expect.equal (extendedTree.createTreeOfSparseMatrix mtx).tree (Leaf(3)) ""


        testCase "sum of Trees" <| fun _ ->
            Expect.equal (quadTree.sum fst snd monoid) sum ""


        testCase "sum of Trees #2" <| fun _ ->
            Expect.throws (fun _ -> quadTree.sum fst errorTree monoid |> ignore) ""


        testCase "reduce" <| fun _ ->
            Expect.equal (quadTree.reduce forReduce 1 2) (Leaf 1) ""


        testCase "scalarMultiply" <| fun _ ->
            Expect.equal (quadTree.scalarMultiply sum 3 semiRing.Mul semiRing.Monoid.Neutral) (Node(Leaf 30, Leaf 30, Leaf 30, Leaf 30)) ""


        testCase "align" <| fun _ ->
            let fst, snd = extendedTree.alignTrees <| extendedTree(1, 1, errorTree) <| extendedTree(2, 2, sum)
            Expect.equal fst.tree (Node(Leaf 404, None, None, None)) ""


        testCase "multiply" <| fun _ ->
            Expect.equal (extendedTree.multiply fstMultTree sndMultTree (SemiRing(semiRing))) resMultTree ""


        testCase "tensorMultiply" <| fun _ ->
            let fst = extendedTree.createTreeOfSparseMatrix (SparseMatrix(1, 1, [Cell(0, 0, 1)]))
            let snd = extendedTree.createTreeOfSparseMatrix (SparseMatrix(2, 2, [Cell(0, 0, 10); Cell(1, 0, 10)]))
            let res = extendedTree.createTreeOfSparseMatrix (SparseMatrix(2, 2, [Cell(0, 0, 10); Cell(1, 0, 10)]))
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
