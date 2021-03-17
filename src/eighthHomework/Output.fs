module Output


open System
open QuadTree
open System.IO
open SparseMatrix


let readToSparseMatrix path =
    let binStrings = File.ReadAllLines path
    let colSize = (binStrings.[0].Split [|' '; '\n'|] ).Length
    let listOfCells =
         [
          for i in 0 .. binStrings.Length - 1 do
              if (binStrings.[i].Split([|' '; '\n'|])).Length <> colSize then failwith "wrong matrix"
              let subStrings = binStrings.[i].Split([|' '; '\n'|])
              for j in 0 .. colSize - 1 do
                  Cell(i, j, subStrings.[j])
          ]
    SparseMatrix(binStrings.Length, (binStrings.[0].Split()).Length, listOfCells)


/// arg: SparseMatrix<string> -> SparseMatrix<int>, using int <| string
let toIntSparse (sparse: SparseMatrix<string>) =
    SparseMatrix(sparse.lineSize, sparse.colSize,
                 List.map ((fun (cell: Cell<string>) -> Cell(cell.line, cell.col, int cell.data))) sparse.content)


let toBoolSparse (sparse: SparseMatrix<string>) =
    let temp = toIntSparse sparse
    SparseMatrix(temp.lineSize, temp.colSize, List.filter (fun cell -> cell.data <> 0) temp.content)


let extTreeToDotAfterTrClosure outFile (exTree: extendedTree<int>) =
    let numberOfVertexes = max exTree.lineSize exTree.colSize
    let arrOfVertexes = Array.init numberOfVertexes (id)
    let localFun lst =  List.map (fun (x, y, z) -> (x, y)) lst
    let originalLst = localFun (exTree |> extendedTree.toSparseMatrix |> SparseMatrix.toListOfCells)
    let closedLst = localFun (extendedTree.transitiveClosure exTree |> extendedTree.toSparseMatrix |> SparseMatrix.toListOfCells)
    let remainedLst = List.filter (fun x -> not (List.contains x originalLst)) closedLst
    let head = ["digraph transitiveClosure"
                "{" ]

    let vertexes =
        [
            for vertex in arrOfVertexes ->
                sprintf
                    "%A[label = \"%A\"]" vertex vertex
        ]

    let originalTransitions =
     [
        for (x, y) in originalLst ->
            sprintf
                "%A -> %A" x y
     ]

    let endLst =
        [
            "}"
        ]

    let newTransitions =
        [
            for (x, y) in remainedLst ->
                sprintf
                    "%A -> %A [color = red]" x y
        ]

    File.WriteAllLines (outFile, head @ vertexes @ originalTransitions @ newTransitions @ endLst )
