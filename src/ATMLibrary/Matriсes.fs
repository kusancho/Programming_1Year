module Matriсes


open QuadTree
open interfaces
open AlgebraicStructure


type QuadTreeMtx<'a, 't when 't: equality and 'a: equality> =
    val mtx: extendedTree<'t>

    new(tree) = {mtx = tree}

    interface IMatrix<'t> with
        member this.fold func acc =
            extendedTree.fold func acc this.mtx

        member this.get(i, j) =
            this.mtx.getByIndex i j

        member this.iteri func =
            extendedTree.iteri func this.mtx

        member this.map (func: 't -> 'a) =
            QuadTreeMtx(extendedTree.map func this.mtx) :> IMatrix<_>
        member this.mapi func =
            QuadTreeMtx(extendedTree.mapi func this.mtx) :> IMatrix<_>
        member this.set(i, j) (value: 't) algStr =
            QuadTreeMtx(this.mtx.setByIndex i j value algStr) :> IMatrix<_>

        member this.tensorMultiply snd algStr =
            QuadTreeMtx(extendedTree.tensorMultiply this.mtx (snd.mtx ()) algStr) :> IMatrix<_, _>

        member this.toBool algStr =
            QuadTreeMtx(extendedTree.toBoolTree this.mtx) :> IMatrix<_, _>

        member this.transitiveClosure algStr =
            QuadTreeMtx(this.mtx.transitiveClosure algStr) :> IMatrix<_, _>

        member this.colSize = this.mtx.colSize

        member this.lineSize = this.mtx.lineSize

