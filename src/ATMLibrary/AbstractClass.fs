module AbstractClass

open QuadTree
open System.Collections.Generic


[<AbstractClass>]
type Matrix<'t, 'a when 'a: equality and 't: equality>() =
   abstract member map: ('t -> 'a) -> Matrix<'a, _>
   abstract member init: int -> int -> (int -> int -> 't) -> Matrix<'t, _>

type QTMatrix<'a, 't when 't: equality and 'a: equality> =
    val tree: extendedTree<'t>
    inherit Matrix<'t, 'a>
    new (tree) = {tree = tree}

    override this.init lineSize colSize func =
        QTMatrix(extendedTree.init lineSize colSize func) :> Matrix<_, _>
    override this.map func =
        QTMatrix(extendedTree.map func this.tree) :> Matrix<_, _>

type NFA<'a, 't when 't: equality and 'a: equality> =
    val start: HashSet<int>
    val final: HashSet<int>
    val transitions: Matrix<'t, 'a>
    new(a, b, c) = {start = a; final = b; transitions = c}


    static member seqToAtm (input: list<_>) =
        let mtx = Matrix()
        let tree = mtx.init (input.Length + 1) (input.Length + 1)
                                     (fun i j -> if i + 1 = j then Set([Smb input.[i]]) else Set.empty<_>) :> Matrix<_, _>
        NFA(HashSet([0]), HashSet([input.Length]), tree)
