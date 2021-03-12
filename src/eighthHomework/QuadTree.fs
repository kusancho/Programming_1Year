module QuadTree


open System
open AlgebraicStructure


let getPowOfTwo number =   // out: smallest int 2^x, such that number <= 2^x
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


type quadTree<'t when 't: equality> =
    | Node of quadTree<'t> * quadTree<'t> * quadTree<'t> * quadTree<'t>
    | Leaf of 't
    | None


    static member sum (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) (algebraStruct: AlgebraicStruct<'t>) =
        let sumOp, neutral =
            match algebraStruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Monoid.Sum, x.Monoid.Neutral
        let rec go fst snd =
            match fst, snd with
            | Leaf a, Leaf b ->
                let res = sumOp a b
                if res = neutral
                then None
                else Leaf res
            | None, a -> a
            | a, None -> a
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                let first, second, third, fourth = go nw1 nw2, go ne1 ne2, go sw1 sw2, go se1 se2
                if first = None && second = None && third = None && fourth = None
                then None
                else Node(go nw1 nw2, go ne1 ne2, go sw1 sw2, go se1 se2)
            | _, _ -> failwith "sizes of tree aren't equal"
        go fstTree sndTree


    static member reduce (tree:quadTree<'t>) need current =
        if need = current
        then tree
        else
            match tree with
            | Node(a, None, None, None) ->
                quadTree.reduce a need (current / 2)
            | Node(None, a, None, None) ->
                quadTree.reduce a need (current / 2)
            | Node(None, None, a, None) ->
                quadTree.reduce a need (current / 2)
            | Node(None, None, None, a) ->
                quadTree.reduce a need (current / 2)
            | _ -> failwith "wrong parameters"


    static member scalarMultiply (tree: quadTree<'t>) scalar multOp =
        match tree with
        | Node(a, b, c, d) ->
            Node(quadTree.scalarMultiply a scalar multOp,
                quadTree.scalarMultiply b scalar multOp,
                quadTree.scalarMultiply c scalar multOp,
                quadTree.scalarMultiply d scalar multOp)
        | Leaf(a) -> Leaf(multOp scalar a)
        | None -> None


type extendedTree<'t when 't: equality> =
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


    override this.GetHashCode() =
        hash (this.lineSize, this.colSize, this.tree)

    override this.Equals(t) =
        match t with
        | :? extendedTree<'t> as t ->
            this.tree = t.tree
            && this.lineSize = t.lineSize

            && this.colSize = t.colSize

            && this.tree = this.tree
        | _ -> false


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


    static member sumExTree (x: extendedTree<'t>) (y: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let monoid =
            match algStruct with
            | Monoid x -> Monoid(x)
            | SemiRing x -> Monoid(x.Monoid)
        if x.colSize = y.colSize && x.lineSize = y.lineSize
        then
            extendedTree(x.lineSize, x.colSize, (quadTree.sum x.tree y.tree monoid))
        else failwith "wrong sizes of exTree's"


    static member createTreeOfSparseMatrix (sparseMatrix: SparseMatrix<'t>) =
        let rec go lineBorder colBorder =
            if SparseMatrix.isEmptyInPlace sparseMatrix lineBorder colBorder
            then None
            else
                match lineBorder, colBorder with
                | (a, b), (c, _) when a = b ->  // coordinate of one Cell
                    Leaf(SparseMatrix.getContent sparseMatrix a c)
                | (a, b), (c, d) ->
                    let lineHalf = a + (b - a) / 2
                    let colHalf = c + (d - c) / 2
                    Node((go (a, lineHalf) (c, colHalf)),          //NW
                         (go (a, lineHalf) (colHalf + 1, d)),      //NE
                         (go (lineHalf + 1, b) (c, colHalf)),      //SW
                         (go (lineHalf + 1, b) (colHalf + 1, d)))  //SE

        let border = sparseMatrix.specSize
        extendedTree(sparseMatrix.lineSize, sparseMatrix.colSize, (go (0, border - 1) (0, border - 1)))


    static member alignTrees (fst: extendedTree<'t>) (snd: extendedTree<'t>) =
        match fst.specSize, snd.specSize with
        | a, b when a > b ->
            fst, extendedTree.createTreeOfSparseMatrix <|
                       (SparseMatrix(fst.lineSize, fst.colSize, (extendedTree.toSparseMatrix snd).content))
        | _, _ ->
            (extendedTree.createTreeOfSparseMatrix <|
                       (SparseMatrix(snd.lineSize, snd.colSize, (extendedTree.toSparseMatrix fst).content))), snd


    static member multiply (fst: extendedTree<'t>) (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        if fst.colSize <> snd.lineSize
        then failwith "wrong sizes of trees (can't multiply)"
        let monoid, multOp =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> Monoid(x.Monoid), x.Mul
        let localSum a b = quadTree.sum a b monoid // sum trees in monoid
        let rec go (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) = // mult for equal tree
            match fstTree, sndTree with
            | Node(a1, b1, c1, d1), Node(a2, b2, c2, d2) ->
                Node((localSum (go a1 a2) (go b1 c2)),
                     (localSum (go a1 b2) (go b1 d2)),
                     (localSum (go c1 a2) (go d1 c2)),
                     (localSum (go c1 b2) (go d1 d2)))
            | Leaf(a), Leaf(b) -> Leaf(multOp a b)
            | None, Leaf(_) -> None
            | Leaf(_), None -> None
            | None, None -> None
            | _, None -> None
            | None, _ -> None
            | _, _ -> failwith "error in multiplication, different depth"
        let resSpecSize = getPowOfTwo <| max fst.lineSize snd.colSize
        if fst.specSize <> snd.specSize
        then
             let fstLocalTree, sndLocalTree = extendedTree.alignTrees fst snd
             let result = go fstLocalTree.tree sndLocalTree.tree
             extendedTree(fst.lineSize, snd.colSize, quadTree.reduce result resSpecSize fstLocalTree.specSize)
        else
            let result = go fst.tree snd.tree
            extendedTree(fst.lineSize, snd.colSize, quadTree.reduce result resSpecSize fst.specSize)


    static member tensorMultiply  (fst: extendedTree<'t>) (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let monoid, multOp =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> Monoid(x.Monoid), x.Mul
        let rec go tree  =
            match tree with
            | Node(a, b, c, d) ->
                Node(go a, go b, go c, go d)
            | Leaf a -> quadTree.scalarMultiply snd.tree a multOp
            | None -> None
        extendedTree(fst.lineSize * snd.lineSize, fst.colSize * snd.colSize, go fst.tree)
