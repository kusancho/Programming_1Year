module Matriсes


open QuadTree
open interfaces
open AlgebraicStructure


type QuadTreeMtx<'a, 't when 't: equality> =
    val mtx: extendedTree<'t>
    val algStr: AlgebraicStruct<'a>

    new(lineSize, colSize, func, algStr) = {mtx = extendedTree.init lineSize colSize func; algStr = algStr}

    interface IMatrix<'t> with
        member this.fold(var0) (var1) = failwith "todo"
        member this.get(var0, var1) = failwith "todo"
        member this.iteri(var0) = failwith "todo"
        member this.map(var0) = failwith "todo"
        member this.mapi(var0) = failwith "todo"
        member this.set(var0, var1) (var2) = failwith "todo"
        member this.tensorMultiply(var0) (var1) = failwith "todo"
        member this.toBool(var0) = failwith "todo"
        member this.transitiveClosure(var0) = failwith "todo"
        member this.colSize = failwith "todo"

        member this.lineSize = failwith "todo"



