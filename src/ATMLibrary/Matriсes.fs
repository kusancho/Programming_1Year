module Matriсes


open QuadTree
open interfaces
open AlgebraicStructure


type QuadTreeMtx<'t, 'a when 't: equality> =
    val tree: extendedTree<'t>
    val algebraicStruct: AlgebraicStruct<'a>

    interface IMatrix<'t> with
        member this.iteri func =
            extendedTree.iteri func this.tree

        member this.map func =
            QuadTreeMtx(extendedTree.map func this.tree, this.algebraicStruct) :> IMatrix<_>

        member this.mapi func =
            QuadTreeMtx(extendedTree.mapi func this.tree, this.algebraicStruct) :> IMatrix<_>

        member this.fold func acc =
            extendedTree.fold func acc this.tree

        member this.toBool algStr =
            QuadTreeMtx(extendedTree.toBoolTree this.tree, algStr) :> IMatrix<_>

        member this.transitiveClosure algStr =
            QuadTreeMtx(this.tree.transitiveClosure algStr, algStr) :> IMatrix<_>

        member this.Item
            with get (i, j) =
                this.tree.getByIndex i j
            and set (i, j) elem = failwith " "

        member this.changeAlgebraicStruct algStr =
            QuadTreeMtx(this.tree, algStr) :> IMatrix<_>


    new(tree, algStr) = {tree = tree; algebraicStruct = algStr}
