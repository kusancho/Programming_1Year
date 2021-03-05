module TestsForEighthHW

open Expecto
open QuadTree


let fst = Node(Leaf(3), None, Leaf(10), None)
let snd = Node(Leaf(7), Leaf(10), None, Leaf(10))
let errorTree = Leaf(404)
let sum = Node(Leaf(10), Leaf(10), Leaf(10), Leaf(10))


[<Tests>]
let testTree =
    testList "Trees functions" [
        testProperty "autoTests toTree/ofTree" <| fun x ->
            let size = (abs x) % 50 + 2
            let sparse = randomIntSparseMatrix size
            let tree = createTreeOfSparseMatrix sparse
            let func (a, b, _) = a, b
            Expect.equal (List.sortBy func (SparseMatrix.toListOfCells sparse)) (List.sortBy func (SparseMatrix.toListOfCells <| extendedTree.toSparseMatrix tree))


        testCase "extreme case #1" <| fun _ ->
            let mtx = SparseMatrix(0, 0, [])
            Expect.equal (createTreeOfSparseMatrix mtx).tree None ""


        testCase "extreme case #2" <| fun _ ->
            let mtx = SparseMatrix(1, 1, [Cell(0, 0, 3)])
            Expect.equal (createTreeOfSparseMatrix mtx).tree (Leaf(3)) ""


        testCase "sum of Trees" <| fun _ ->
            Expect.equal (sumTree fst snd) sum ""


        testCase "sum of Trees #2" <| fun _ ->
            Expect.throws (fun _ -> sumTree fst errorTree |> ignore) "" ]


let mtx = SparseMatrix(3, 2, [Cell(0,0, 100); Cell(2, 1, 101)])


[<Tests>]
let testSparseMatrix =
    testList "SparseMatrix functions" [
            testCase "getIntContent" <| fun _ ->
                Expect.equal (SparseMatrix.getIntContent mtx 0 0) 100 ""


            testCase "getIntContent throw" <| fun _ ->
                Expect.throws (fun _ -> SparseMatrix.getIntContent mtx 1 0 |> ignore) ""


            testCase "toLisOfCells" <| fun _ ->
                Expect.equal (SparseMatrix.toListOfCells mtx) [(0, 0, 100); (2, 1, 101)] ""


            testCase "isEmptyOnPlace" <| fun _ ->
                Expect.equal (SparseMatrix.isEmptyInPlace mtx (0, 0) (0, 0)) false ""


            testCase "isEmptyOnPlace#2" <| fun _ ->
                Expect.equal (SparseMatrix.isEmptyInPlace mtx (1, 1) (1, 1)) true ""  ]
