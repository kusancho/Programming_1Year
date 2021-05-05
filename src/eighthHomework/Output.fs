module Output


open System
open QuadTree
open System.IO
open SparseMatrix
open AlgebraicStructure


let readToSparseMatrix path  =
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
    SparseMatrix(temp.lineSize, temp.colSize, List.filter (fun cell -> cell.data <> 1) temp.content)


let extTreeToDotAfterTrClosure outFile (exTree: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
    let numberOfVertexes = max exTree.lineSize exTree.colSize
    let arrOfVertexes = Array.init numberOfVertexes (id)
    let originalLst = (exTree.toSparseMatrix |> SparseMatrix.toListOfCells)
    let originCort = List.map (fun (x, y, z) -> (x, y)) originalLst
    let closedLst = (exTree.transitiveClosure algStruct).toSparseMatrix |> SparseMatrix.toListOfCells
    let remainedLst = List.filter (fun (x, y, _) -> not (List.contains (x, y) originCort)) closedLst
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
        for (x, y, z) in originalLst ->
            sprintf
                "%A -> %A [label = \"%A\"]" x y z
     ]

    let endLst =
        [
            "}"
        ]

    let newTransitions =
        [
            for (x, y, z) in remainedLst ->
                sprintf
                    "%A -> %A [color = red] [label = \"%A\"]" x y z
        ]

    File.WriteAllLines (outFile, head @ vertexes @ originalTransitions @ newTransitions @ endLst )
