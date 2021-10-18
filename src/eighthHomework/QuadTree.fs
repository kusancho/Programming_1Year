module QuadTree


open AlgebraicStructure
open SparseMatrix
open System.Collections.Generic
open Interface


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
            | Leaf(a) -> Leaf(multOp scalar a).noneCheck neutral
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
            | Node(a, b, c, d) -> Node(go a, go b, go c, go d).noneCheck neutral
            | Leaf(a) -> Leaf(a).noneCheck neutral
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


    member this.getByIndex i j algStr =
        let neutral =
            match algStr with
            | Monoid x -> x.Neutral
            | SemiRing x -> x.Monoid.Neutral
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
            | None -> neutral
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

        let lineDeviation = snd.specSize - snd.lineSize
        let colDeviation = snd.specSize - snd.colSize
        let tempSparse = extendedTree(this.lineSize * snd.specSize, this.colSize * snd.specSize, go this.tree).toSparseMatrix
        let cells = List.map (fun (x: Cell<'t>) -> Cell(x.line - (x.line / snd.specSize) * lineDeviation,
                                                    x.col - (x.col / snd.specSize) * colDeviation,
                                                    x.data)) tempSparse.content
        extendedTree.createTreeOfSparseMatrix algStruct (SparseMatrix(this.lineSize * snd.lineSize, this.colSize * snd.colSize, cells))


    member this.toBoolTree =
        let rec go tree =
            match tree with
            | Node(a, b, c, d) -> Node(go a, go b, go c, go d)
            | Leaf _ -> Leaf true
            | None -> None
        extendedTree(this.lineSize, this.colSize, go this.tree)


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


    member this.parallelMultiply (snd: extendedTree<'t>) (algStruct: AlgebraicStruct<'t>) depth =
        if this.colSize <> snd.lineSize
        then failwith "wrong sizes of trees (can't multiply)"
        let monoid, multOp, neutral =
            match algStruct with
            | Monoid x -> failwith "we can't multiply in monoid"
            | SemiRing x -> Monoid(x.Monoid), x.Mul, x.Monoid.Neutral
        let localSum (a: quadTree<_>) b = a.plus b monoid       // sum trees in monoid

        let rec go (fstTree: quadTree<'t>) (sndTree: quadTree<'t>) depth = // mult for equal tree
            let createParallelLeaf a b c d = async { return (localSum (go a b (depth - 1)) (go c d (depth - 1))) }
            let createLeaf a b c d = (localSum (go a b depth) (go c d depth))
            match fstTree, sndTree with
                | Node(a1, b1, c1, d1), Node(a2, b2, c2, d2) ->
                        if depth = 0
                        then
                            let n1 = createParallelLeaf a1 a2 b1 c2
                            let n2 = createParallelLeaf a1 b2 b1 d2
                            let n3 = createParallelLeaf c1 a2 d1 c2
                            let n4 = createParallelLeaf c1 b2 d1 d2
                            let leaves = [n1; n2; n3; n4] |> Async.Parallel |> Async.RunSynchronously
                            Node(leaves.[0], leaves.[1], leaves.[2], leaves.[3]).noneCheck neutral
                        else
                            Node(createLeaf a1 a2 b1 c2, createLeaf a1 b2 b1 d2,
                                 createLeaf c1 a2 d1 c2, createLeaf  c1 b2 d1 d2).noneCheck neutral
                | _, None -> None
                | None, _ -> None
                | Leaf(a), Leaf(b) -> Leaf(multOp a b).noneCheck neutral
                | _, _ -> failwith "error in multiplication, different size"

        let resSpecSize = getPowOfTwo <| max this.lineSize snd.colSize
        if this.specSize <> snd.specSize
        then
             let fstLocalTree, sndLocalTree = extendedTree.alignTrees this snd algStruct
             let result = go fstLocalTree.tree sndLocalTree.tree depth
             extendedTree(this.lineSize, snd.colSize, result.reduce resSpecSize fstLocalTree.specSize)
        else
            let result = go this.tree snd.tree depth
            extendedTree(this.lineSize, snd.colSize, result.reduce resSpecSize this.specSize)


    member this.toArray neutral =
        let res = [| for _ in 0 .. this.lineSize - 1 -> [| for _ in 0 .. this.colSize - 1 -> neutral |] |]
        List.iter (fun (cell: Cell<_>) -> res.[cell.line].[cell.col] <- cell.data) this.toSparseMatrix.content
        res


    interface IMatrix<'t> with
        member this.colSize =
            this.colSize

        member this.fold acc func =
            extendedTree.fold acc func this

        member this.get(i, j, algStr) =
            this.getByIndex i j algStr

        member this.iteri func =
            extendedTree.iteri func this

        member this.lineSize =
            this.lineSize

        member this.map func =
            extendedTree.map func this :> IMatrix<_>

        member this.mapi func =
            extendedTree.mapi func this :> IMatrix<_>

        member this.tensorMultiply (snd: IMatrix<'t>) algStr =
            match snd with
            | :? extendedTree<'t> as x ->
                this.tensorMultiply x algStr :> IMatrix<_>
            | _ -> failwith " "

        member this.toBool neutral =
            (extendedTree.clearNeutral neutral this).toBoolTree :> IMatrix<_>

        member this.transitiveClosure algStr =
            this.transitiveClosure algStr :> IMatrix<_>
