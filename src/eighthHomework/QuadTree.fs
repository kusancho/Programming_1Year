module QuadTree


open System


let getPowOfTwo number =   // out: smallest int 2^x, such that number <= 2^x
    let mutable aproxNum = 1
    while number > aproxNum do
        aproxNum <- aproxNum * 2
    aproxNum


type Cell<'t> =
    val line: int
    val col: int
    val data: 't
    new(line, col, data) = {
        line = line
        col = col
        data = data
    }


type SparseMatrix<'t> =
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
        let func acc (cell: Cell<'t>) = acc @ [cell.line, cell.col, cell.data]
        List.fold func [] mtx.content


    static member isEmptyInPlace (sparseMatrix: SparseMatrix<'t>) lineBorders colBorders =
        let tempFun acc (cell: Cell<'t>)  =
            let line, col = cell.line, cell.col
            match lineBorders, colBorders with
            | (a, b), (c, d) when line <= b && line >= a && col <= d && col >= c -> false
            | _ -> acc
        List.fold tempFun true sparseMatrix.content


    static member getIntContent (sparseMatrix: SparseMatrix<'t>) line col =
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


type quadTree<'t> =
    | Node of quadTree<'t> * quadTree<'t> * quadTree<'t> * quadTree<'t>
    | Leaf of 't
    | None


let rec sumTree fst snd =
    match fst, snd with
    | Leaf a, Leaf b ->
        if a + b = 0
        then None
        else Leaf(a + b)
    | None, a -> a
    | a, None -> a
    | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
        let first, second, third, fourth = sumTree nw1 nw2, sumTree ne1 ne2, sumTree sw1 sw2, sumTree se1 se2
        if first = None && second = None && third = None && fourth = None
        then None
        else Node(sumTree nw1 nw2, sumTree ne1 ne2, sumTree sw1 sw2, sumTree se1 se2)
    | _, _ -> failwith "sizes of tree aren't equal"



type extendedTree<'t> =
    val lineSize: int
    val colSize: int
    val specSize: int
    val tree: quadTree<'t>
    new(line, col, tree) = {
        lineSize = line
        colSize = col
        specSize = getPowOfTwo <| max line col
        tree = tree
    }


    static member toSparseMatrix (exTree: extendedTree<'t>) =
        let size = exTree.specSize
        let rec go lineH lineL colH colL tree =
            match tree with
            | Leaf(a) ->
                [Cell(lineH, colH, a)]
            | Node(a, b, c, d) ->
                let lineHalf = lineH + (lineL - lineH) / 2
                let colHalf = colH + (colL - colH) / 2
                ((go lineH lineHalf colH colHalf a) @
                 (go lineH lineHalf (colHalf + 1) colL b) @
                 (go (lineHalf + 1) lineL colH colHalf c) @
                 (go (lineHalf + 1) lineL (colHalf + 1) colL d))
            | None -> []
        SparseMatrix(exTree.lineSize, exTree.colSize, go 0 (size - 1) 0 (size - 1) exTree.tree)


let rec sumExTree (x: extendedTree<int>) (y: extendedTree<int>) =
    if x.colSize = y.colSize && x.lineSize = y.lineSize
    then
        extendedTree(x.lineSize, x.colSize, sumTree x.tree y.tree)
    else failwith "wrong sizes of exTree's"


let rec createTreeOfSparseMatrix sparseMatrix =
    let rec go lineBorder colBorder =
        if SparseMatrix.isEmptyInPlace sparseMatrix lineBorder colBorder
        then None
        else
            match lineBorder, colBorder with
            | (a, b), (c, _) when a = b ->  // coordinate of one Cell
                Leaf(SparseMatrix.getIntContent sparseMatrix a c)
            | (a, b), (c, d) ->
                let lineHalf = a + (b - a) / 2
                let colHalf = c + (d - c) / 2
                Node((go (a, lineHalf) (c, colHalf)),          //NW
                     (go (a, lineHalf) (colHalf + 1, d)),      //NE
                     (go (lineHalf + 1, b) (c, colHalf)),      //SW
                     (go (lineHalf + 1, b) (colHalf + 1, d)))  //SE

    let border = sparseMatrix.specSize
    extendedTree(sparseMatrix.lineSize, sparseMatrix.colSize, (go (0, border - 1) (0, border - 1)))
