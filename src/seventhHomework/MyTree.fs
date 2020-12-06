module MyTree

open MyList

type MyTree<'T> =
    | Node of int * MyList<MyTree<'T>>
    | Leaf of 'T


