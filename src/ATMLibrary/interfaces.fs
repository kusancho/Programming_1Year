module interfaces


open AlgebraicStructure


type IMatrix<'t when 't: equality> =
    abstract member map: ('t -> 'a) -> IMatrix<'a>

    abstract member iteri: (int -> int -> 't -> unit) -> unit

    abstract member mapi: (int -> int -> 't -> 'a) -> IMatrix<'a>

    abstract member transitiveClosure: AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member toBool: AlgebraicStruct<bool> -> IMatrix<bool>

    abstract member fold: ('a -> 't -> 'a) -> 'a -> 'a

    abstract member set: int * int -> 't -> IMatrix<'t>

    abstract member get: int * int -> 't

    abstract member changeAlgebraicStruct: AlgebraicStruct<'a> -> IMatrix<'t>






