module QuadTree


open AlgebraicStructure
open SparseMatrix
open System.Collections.Generic


type quadTree<'t when 't: equality> =
    | Node of quadTree<'t> * quadTree<'t> * quadTree<'t> * quadTree<'t>
    | Leaf of 't
    | None

    static member noneCheck neutral (tree: quadTree<'t>) =
        match tree with
        | None -> None
        | Node(None, None, None, None) -> None
        | Leaf a when a = neutral -> None
        | a -> a


    static member sum (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) (algebraStruct: AlgebraicStruct<'t>) =
        let sumOp, neutral =
            match algebraStruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Monoid.Sum, x.Monoid.Neutral
        let rec go fst snd =
            match fst, snd with
            | None, a -> a
            | a, None -> a
            | Leaf a, Leaf b -> quadTree.noneCheck neutral <| Leaf(sumOp a b)
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                quadTree.noneCheck neutral <| Node((go nw1 nw2), (go ne1 ne2), (go sw1 sw2), (go se1 se2))
            | _, _ -> failwith "sizes of tree aren't equal"
        go fstTree sndTree


    static member reduce (tree:quadTree<'t>) need current =
        if current < need then failwith "reduce"
        elif need = current
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
            | Leaf a -> Leaf(a)
            | None -> None
            | _ -> failwith "wrong parameters"


    static member scalarMultiply (tree: quadTree<'t>) scalar multOp neutral =
        if scalar = neutral
        then None
        else
            match tree with
            | Node(a, b, c, d) ->
                quadTree.noneCheck neutral <| Node(quadTree.scalarMultiply a scalar multOp neutral,
                    quadTree.scalarMultiply b scalar multOp neutral,
                    quadTree.scalarMultiply c scalar multOp neutral,
                    quadTree.scalarMultiply d scalar multOp neutral)
            | Leaf(a) -> quadTree.noneCheck neutral <| Leaf(multOp scalar a)
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

    static member private forceCreate (sparseMatrix: SparseMatrix<'t>) =
        let rec go lineBorder colBorder =
            if SparseMatrix.isEmptyInPlace sparseMatrix lineBorder colBorder
            then None
            else
                match lineBorder, colBorder with
                | (a, b), (c, d) when a = b ->  // coordinate of one Cell
                    Leaf(SparseMatrix.getContent sparseMatrix a c)
                | (a, b), (c, d) ->
                    let lineHalf = a + (b - a) / 2
                    let colHalf = c + (d - c) / 2
                    Node((go (a, lineHalf) (c, colHalf)),                                 //NW
                         (go (a, lineHalf) (colHalf + 1, d)),                             //NE
                         (go (lineHalf + 1, b) (c, colHalf)),                             //SW
                         (go (lineHalf + 1, b) (colHalf + 1, d)))                         //SE

        let border = sparseMatrix.specSize
        extendedTree(sparseMatrix.lineSize, sparseMatrix.colSize, (go (0, border - 1) (0, border - 1)))


    member this.toSparseMatrix =
        let size = this.specSize
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
        SparseMatrix(this.lineSize, this.colSize, go 0 (size - 1) 0 (size - 1) this.tree)


    static member init lineSize colSize (func: int -> int -> 't) =
        let acc = HashSet<Cell<'t>>()
        for i in 0 .. lineSize - 1 do
            for j in 0 .. colSize - 1 do
                acc.Add <| Cell(i, j, func i j) |> ignore
        extendedTree.forceCreate (SparseMatrix(lineSize, colSize, List.ofSeq acc))


    static member clearNeutral neutral (tree: extendedTree<'t>) =
        let rec go tree =
            match tree with
            | Node(a, b, c, d) ->
                quadTree.noneCheck neutral <| Node(go a,
                                                   go b,
                                                   go c,
                                                   go d)
            | Leaf(a) -> quadTree.noneCheck neutral <| Leaf(a)
            | None -> None
        extendedTree(tree.lineSize, tree.colSize, go tree.tree)


    member this.fillNeutral (neutral: 't) =
        let content = this.toSparseMatrix.content
        let flagLst = [for item in content -> (item.line, item.col)]
        let contWithNeutrals = [for i in 0 .. this.lineSize - 1 do
                                    for j in 0 .. this.colSize - 1 do
                                        if not <| List.contains (i, j) flagLst
                                        then Cell(i, j, neutral)]
        extendedTree.forceCreate <| SparseMatrix(this.lineSize, this.colSize, List.concat [contWithNeutrals; content])


    /// if element is None, it will break
    member this.getByIndex i j  =
        let size = this.specSize
        if this.lineSize <= i || this.colSize <= j || i < 0 || j < 0
        then failwith "Index was outside the bounds of the array."
        let rec go lineH lineL colH colL tree =
            match tree with
            | Leaf(a) -> a
            | Node(a, b, c, d) ->
                let lineHalf = lineH + (lineL - lineH) / 2
                let colHalf = colH + (colL - colH) / 2
                match i, j with
                | i, j when i <= lineHalf && j <= colHalf -> go lineH lineHalf colH colHalf a
                | i, j when i <= lineHalf && j > colHalf -> go lineH lineHalf (colHalf + 1) colL b
                | i, j when i > lineHalf && j <= colHalf -> go (lineHalf + 1) lineL colH colHalf c
                | i, j when i > lineHalf && j > colHalf -> go (lineHalf + 1) lineL (colHalf + 1) colL d
                | _ -> failwith "Index was outside the bounds of the array."
            | None -> failwith "read member description"
        go 0 (size - 1) 0 (size - 1) this.tree


    static member iteri (func: int -> int -> 't -> unit) (tree: extendedTree<'t>) =
        let size = tree.specSize
        let rec go lineH lineL colH colL tree =
            match tree with
            | Leaf(a) ->
                func lineH colH a
            | Node(a, b, c, d) ->
                let lineHalf = lineH + (lineL - lineH) / 2
                let colHalf = colH + (colL - colH) / 2
                (go lineH lineHalf colH colHalf a)
                (go lineH lineHalf (colHalf + 1) colL b)
                (go (lineHalf + 1) lineL colH colHalf c)
                (go (lineHalf + 1) lineL (colHalf + 1) colL d)
            | None -> ()
        go 0 (size - 1) 0 (size - 1) tree.tree


    /// not enough safe, possible situation:
    /// Node(None, None, None, Leaf(1)) -> Node(None, None, None, Leaf(0))
    /// expected: None
    static member mapi (func: int -> int -> 't -> 'a) (exTree: extendedTree<'t>) =
        let size = exTree.specSize
        let rec go lineH lineL colH colL tree =
            match tree with
            | Leaf(a) -> Leaf(func lineH colH a)
            | Node(a, b, c, d) ->
                let lineHalf = lineH + (lineL - lineH) / 2
                let colHalf = colH + (colL - colH) / 2
                Node((go lineH lineHalf colH colHalf a),
                     (go lineH lineHalf (colHalf + 1) colL b),
                     (go (lineHalf + 1) lineL colH colHalf c),
                     (go (lineHalf + 1) lineL (colHalf + 1) colL d))
            | None -> None
        extendedTree(exTree.lineSize, exTree.colSize, go 0 (size - 1) 0 (size - 1) exTree.tree)


    /// application func to non None elements
    static member fold (func: 'a -> 't -> 'a) (acc: 'a) (tree: extendedTree<'t>) =
        let rec go tree acc =
            match tree with
            | Node(a, b, c, d) ->
                go d (go c (go b (go a acc)))
            | Leaf(a) -> func acc a
            | None -> acc
        go tree.tree acc


    /// application func to non None elements
    static member map (func: 't -> 'a) (exTree: extendedTree<'t>) =
        let size = exTree.specSize
        let rec go lineH lineL colH colL tree =
            match tree with
            | Leaf(a) -> Leaf(func a)
            | Node(a, b, c, d) ->
                let lineHalf = lineH + (lineL - lineH) / 2
                let colHalf = colH + (colL - colH) / 2
                Node((go lineH lineHalf colH colHalf a),
                     (go lineH lineHalf (colHalf + 1) colL b),
                     (go (lineHalf + 1) lineL colH colHalf c),
                     (go (lineHalf + 1) lineL (colHalf + 1) colL d))
            | None -> None
        extendedTree(exTree.lineSize, exTree.colSize, go 0 (size - 1) 0 (size - 1) exTree.tree)

    static member sumExTree (x: extendedTree<'t>) (y: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let monoid =
            match algStruct with
            | Monoid x -> Monoid(x)
            | SemiRing x -> Monoid(x.Monoid)
        if x.colSize = y.colSize && x.lineSize = y.lineSize
        then
            extendedTree(x.lineSize, x.colSize, (quadTree.sum x.tree y.tree monoid))
        else failwith "wrong sizes of exTree's"


    static member createTreeOfSparseMatrix (algStruct: AlgebraicStruct<'t>) (sparseMatrix: SparseMatrix<'t>) =
        let neutral =
            match algStruct with
            | Monoid x -> x.Neutral
            | SemiRing x -> x.Monoid.Neutral
        let rec go lineBorder colBorder =
            if SparseMatrix.isEmptyInPlace sparseMatrix lineBorder colBorder
            then None
            else
                match lineBorder, colBorder with
                | (a, b), (c, d) when a = b ->  // coordinate of one Cell
                    quadTree.noneCheck neutral <| Leaf(SparseMatrix.getContent sparseMatrix a c)
                | (a, b), (c, d) ->
                    let lineHalf = a + (b - a) / 2
                    let colHalf = c + (d - c) / 2
                    quadTree.noneCheck neutral <| Node((go (a, lineHalf) (c, colHalf)),   //NW
                         (go (a, lineHalf) (colHalf + 1, d)),                             //NE
                         (go (lineHalf + 1, b) (c, colHalf)),                             //SW
                         (go (lineHalf + 1, b) (colHalf + 1, d)))                         //SE

        let border = sparseMatrix.specSize
        extendedTree(sparseMatrix.lineSize, sparseMatrix.colSize, (go (0, border - 1) (0, border - 1)))


    static member alignTrees (fst: extendedTree<'t>) (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        match fst.specSize, snd.specSize with
        | a, b when a > b ->
            fst, extendedTree.createTreeOfSparseMatrix algStruct <|
                       (SparseMatrix(fst.lineSize, fst.colSize, snd.toSparseMatrix.content))
        | _, _ ->
            (extendedTree.createTreeOfSparseMatrix algStruct <|
                       (SparseMatrix(snd.lineSize, snd.colSize, fst.toSparseMatrix.content))), snd


    static member multiply (fst: extendedTree<'t>) (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        if fst.colSize <> snd.lineSize
        then failwith "wrong sizes of trees (can't multiply)"
        let monoid, multOp, neutral =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> Monoid(x.Monoid), x.Mul, x.Monoid.Neutral
        let localSum a b = quadTree.sum a b monoid // sum trees in monoid
        let rec go (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) = // mult for equal tree
            match fstTree, sndTree with
            | Node(a1, b1, c1, d1), Node(a2, b2, c2, d2) ->
                quadTree.noneCheck neutral <| Node((localSum (go a1 a2) (go b1 c2)), (localSum (go a1 b2) (go b1 d2)),
                                    (localSum (go c1 a2) (go d1 c2)), (localSum (go c1 b2) (go d1 d2)))
            | _, None -> None
            | None, _ -> None
            | Leaf(a), Leaf(b) -> quadTree.noneCheck neutral <| Leaf(multOp a b)
            | _, _ -> failwith "error in multiplication, different depth"
        let resSpecSize = getPowOfTwo <| max fst.lineSize snd.colSize
        if fst.specSize <> snd.specSize
        then
             let fstLocalTree, sndLocalTree = extendedTree.alignTrees fst snd algStruct
             let result = go fstLocalTree.tree sndLocalTree.tree
             extendedTree(fst.lineSize, snd.colSize, quadTree.reduce result resSpecSize fstLocalTree.specSize)
        else
            let result = go fst.tree snd.tree
            extendedTree(fst.lineSize, snd.colSize, quadTree.reduce result resSpecSize fst.specSize)


    static member tensorMultiply  (fst: extendedTree<'t>) (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let neutral, multOp =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> x.Monoid.Neutral, x.Mul
        let rec go tree  =
            match tree with
            | Node(a, b, c, d) ->
                quadTree.noneCheck neutral <| Node((go a), (go b), (go c), (go d))
            | Leaf a -> quadTree.scalarMultiply snd.tree a multOp neutral
            | None -> None
        let lineDeviation = snd.specSize - snd.lineSize
        let colDeviation = snd.specSize - snd.colSize
        let tempSparse = extendedTree(fst.lineSize * snd.specSize, fst.colSize * snd.specSize, go fst.tree).toSparseMatrix
        let cells = List.map (fun (x: Cell<'t>) -> Cell(x.line - (x.line / snd.specSize) * lineDeviation,
                                                    x.col - (x.col / snd.specSize) * colDeviation,
                                                    x.data)) tempSparse.content
        extendedTree.createTreeOfSparseMatrix algStruct (SparseMatrix(fst.lineSize * snd.lineSize, fst.colSize * snd.colSize, cells))


    static member toBoolTree (exTree: extendedTree<'t>) =
        let rec go tree =
            match tree with
            | Node(a, b, c, d) -> Node(go a, go b, go c, go d)
            | Leaf _ -> Leaf true
            | None -> None
        extendedTree(exTree.lineSize, exTree.colSize, go exTree.tree)


    member this.transitiveClosure (algStruct: AlgebraicStruct<'t>) =
        let monoid, semiRing =
            match algStruct with
            | Monoid x -> failwith "can't multiply in monoid"
            | SemiRing x -> Monoid x.Monoid, SemiRing x
        let n = max this.lineSize this.colSize
        let rec accumulate curr iter acc =
            if iter = 0
            then acc
            else
                let temp = extendedTree.multiply this curr semiRing
                accumulate temp (iter - 1) (acc @ [temp])
        let lstOfMtx = accumulate this n []
        List.fold (fun acc x -> extendedTree.sumExTree acc x monoid) this lstOfMtx
