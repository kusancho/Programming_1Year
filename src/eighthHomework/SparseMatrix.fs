module SparseMatrix


open System


/// out: smallest int 2^x, such that number <= 2^x
let getPowOfTwo number =
    let mutable aproxNum = 1
    while number > aproxNum do
        aproxNum <- aproxNum * 2
    aproxNum


type Cell<'t when 't: equality> =
    val line: int
    val col: int
    val data: 't
    new(line, col, data) = {
        line = line
        col = col
        data = data
    }


type SparseMatrix<'t when 't: equality> =
    val lineSize: int
    val colSize: int
    val specSize: int
    val content: list<Cell<'t>>
    new(line, col, content) = {
        lineSize = line
        colSize = col
        specSize = getPowOfTwo <| max line col
        content = content
    }


    static member toListOfCells (mtx: SparseMatrix<'t>) =
        List.map (fun (cell: Cell<'t>) -> (cell.line, cell.col, cell.data)) mtx.content


    static member isEmptyInPlace (sparseMatrix: SparseMatrix<'t>) lineBorders colBorders =
        let tempFun acc (cell: Cell<'t>)  =
            let line, col = cell.line, cell.col
            match lineBorders, colBorders with
            | (a, b), (c, d) when line <= b && line >= a && col <= d && col >= c -> false
            | _ -> acc
        List.fold tempFun true sparseMatrix.content


    static member getContent (sparseMatrix: SparseMatrix<'t>) line col =
        let mutable flag = false
        let mutable a = sparseMatrix.content.Head
        for item in sparseMatrix.content do
            let lineC = item.line
            let colC = item.col
            if line = lineC && col = colC
            then
                flag <- true
                a <- item
        if flag then a.data
        else failwith "element doesn't exist"


let randomIntSparseMatrix size =
    let rnd = Random()
    let temp = Array2D.init size size (fun _ _ -> rnd.Next(0, 2))
    let arr = Array2D.map (fun x -> if x > 0 then Random().Next(1, 1000) else x) temp
    let listOfCells = [for i in 0 .. (arr.GetLength 0) - 1 do
                           for j in 0 .. (arr.GetLength 1) - 1 do
                               if arr.[i, j] <> 0 then Cell(i, j, arr.[i, j])
                               ]
    SparseMatrix(arr.GetLength 0, arr.GetLength 1, listOfCells)
