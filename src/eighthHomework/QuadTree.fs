module QuadTree


open AlgebraicStructure
open SparseMatrix


type quadTree<'t when 't: equality> =
    | Node of quadTree<'t> * quadTree<'t> * quadTree<'t> * quadTree<'t>
    | Leaf of 't
    | None

    member this.noneCheck neutral =
        match this with
        | None -> None
        | Node(None, None, None, None) -> None
        | Leaf a when a = neutral -> None
        | a -> a


    member this.plus (sndTree: quadTree<'t>) (algebraStruct: AlgebraicStruct<'t>) =
        let sumOp, neutral =
            match algebraStruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Monoid.Sum, x.Monoid.Neutral
        let rec go fst snd =
            match fst, snd with
            | None, a -> a
            | a, None -> a
            | Leaf a, Leaf b -> Leaf(sumOp a b).noneCheck neutral
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                Node((go nw1 nw2), (go ne1 ne2), (go sw1 sw2), (go se1 se2)).noneCheck neutral
            | _, _ -> failwith "sizes of tree aren't equal"
        go this sndTree


    member this.reduce need current =
        if current < need then failwith "reduce"
        elif need = current
        then this
        else
            match this with
            | Node(a, None, None, None) ->
                a.reduce need (current / 2)
            | Node(None, a, None, None) ->
                a.reduce need (current / 2)
            | Node(None, None, a, None) ->
                a.reduce need (current / 2)
            | Node(None, None, None, a) ->
                a.reduce need (current / 2)
            | Leaf a -> Leaf(a)
            | None -> None
            | _ -> failwith "wrong parameters"


    member this.scalarMultiply scalar multOp neutral =
        if scalar = neutral
        then None
        else
            match this with
            | Node(a, b, c, d) ->
                Node(a.scalarMultiply scalar multOp neutral,
                     b.scalarMultiply scalar multOp neutral,
                     c.scalarMultiply scalar multOp neutral,
                     d.scalarMultiply scalar multOp neutral).noneCheck neutral
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


    member this.plus (y: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let monoid =
            match algStruct with
            | Monoid x -> Monoid(x)
            | SemiRing x -> Monoid(x.Monoid)
        if this.colSize = y.colSize && this.lineSize = y.lineSize
        then
            extendedTree(this.lineSize, this.colSize, (this.tree.plus y.tree monoid))
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
                    Leaf(SparseMatrix.getContent sparseMatrix a c).noneCheck neutral
                | (a, b), (c, d) ->
                    let lineHalf = a + (b - a) / 2
                    let colHalf = c + (d - c) / 2
                    Node((go (a, lineHalf) (c, colHalf)),                                 //NW
                         (go (a, lineHalf) (colHalf + 1, d)),                             //NE
                         (go (lineHalf + 1, b) (c, colHalf)),                             //SW
                         (go (lineHalf + 1, b) (colHalf + 1, d))).noneCheck neutral       //SE

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


    member this.multiply (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        if this.colSize <> snd.lineSize
        then failwith "wrong sizes of trees (can't multiply)"
        let monoid, multOp, neutral =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> Monoid(x.Monoid), x.Mul, x.Monoid.Neutral
        let localSum (a: quadTree<_>) b = a.plus b monoid       // sum trees in monoid
        let rec go (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) = // mult for equal tree
            match fstTree, sndTree with
            | Node(a1, b1, c1, d1), Node(a2, b2, c2, d2) ->
                Node((localSum (go a1 a2) (go b1 c2)), (localSum (go a1 b2) (go b1 d2)),
                     (localSum (go c1 a2) (go d1 c2)), (localSum (go c1 b2) (go d1 d2))).noneCheck neutral
            | _, None -> None
            | None, _ -> None
            | Leaf(a), Leaf(b) -> Leaf(multOp a b).noneCheck neutral
            | _, _ -> failwith "error in multiplication, different depth"
        let resSpecSize = getPowOfTwo <| max this.lineSize snd.colSize
        if this.specSize <> snd.specSize
        then
             let fstLocalTree, sndLocalTree = extendedTree.alignTrees this snd algStruct
             let result = go fstLocalTree.tree sndLocalTree.tree
             extendedTree(this.lineSize, snd.colSize, result.reduce resSpecSize fstLocalTree.specSize)
        else
            let result = go this.tree snd.tree
            extendedTree(this.lineSize, snd.colSize, result.reduce resSpecSize this.specSize)


    member this.tensorMultiply (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) =
        let neutral, multOp =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> x.Monoid.Neutral, x.Mul
        let rec go tree  =
            match tree with
            | Node(a, b, c, d) ->
                Node((go a), (go b), (go c), (go d)).noneCheck neutral
            | Leaf a -> (snd.tree.scalarMultiply a multOp neutral).noneCheck neutral
            | None -> None
        extendedTree(this.lineSize * snd.colSize, this.colSize * snd.colSize, go this.tree)


    member private this.toBoolTree (exTree: extendedTree<'t>) =
        let rec go tree =
            match tree with
            | Node(a, b, c, d) -> Node(go a, go b, go c, go d)
            | Leaf a -> Leaf 1
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
                let temp =  this.multiply curr semiRing
                accumulate temp (iter - 1) (acc @ [temp])
        let lstOfMtx = accumulate this n []
        List.fold (fun (acc: extendedTree<_>) x -> acc.plus x monoid) this lstOfMtx
