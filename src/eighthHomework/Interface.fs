module Interface


open AlgebraicStructure


type IMatrix<'t when 't: equality> =

    abstract member map: ('t -> 'a) -> IMatrix<'a>

    abstract member iteri: (int -> int -> 't -> unit) -> unit

    abstract member mapi: (int -> int -> 't -> 'a) -> IMatrix<'a>

    abstract member transitiveClosure: AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member toBool: 't -> IMatrix<bool>

    abstract member fold: ('acc -> 't -> 'acc) -> 'acc -> 'acc

    abstract member get: int * int * AlgebraicStruct<'t> -> 't

    abstract member tensorMultiply: IMatrix<'t> -> AlgebraicStruct<'t> -> IMatrix<'t>

    abstract member lineSize: int

    abstract member colSize: int
