module interfaces


open AlgebraicStructure
open QuadTree

// add there to .......
type support<'t when 't: equality> =
    | ExtendedTree of extendedTree<'t>

type IMatrix<'t when 't: equality> =
    abstract member extendedTree: extendedTree<'t>
    abstract member map: ('t -> 'a) -> IMatrix<'a>

    abstract member iteri: (int -> int -> 't -> unit) -> unit

    abstract member mapi: (int -> int -> 't -> 'a) -> IMatrix<'a>

    abstract member transitiveClosure: AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member toBool: IMatrix<bool>

    abstract member fold: ('g -> 't -> 'g) -> 'g -> 'g

    abstract member set: int * int -> 't -> AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member get: int * int -> 't

    abstract member tensorMultiply: IMatrix<'t> -> AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member lineSize: int

    abstract member colSize: int
