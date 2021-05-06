module Interface

open System.Collections.Generic

[<AbstractClass>]
type advancedMatrix<'elem>(a, b, c) =
    abstract member init: (int, int, (int -> int -> 'elem))
    (*abstract Item: int * int -> 'elem with get, set
    abstract Size: int * int with get


    // static members

    abstract member iteri: advancedMatrix<'elem> -> unit
    abstract member transitiveClosure: advancedMatrix<'elem> -> advancedMatrix<'elem>
    abstract member map: ('elem -> 'a) * advancedMatrix<'elem> -> advancedMatrix<'a>
    abstract member toBool: advancedMatrix<'elem> -> advancedMatrix<bool>*)


