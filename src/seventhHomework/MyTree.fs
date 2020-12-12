module MyTree

open MyList

type MyTree<'T> =
    | Node of 'T * MyList<MyTree<'T>>
    | Leaf of 'T

    static member fold func acc (tree: MyTree<'T>) =
        match tree with
        | Leaf x -> func acc x
        | Node (x, tail) -> MyList.fold (fun acc t -> MyTree.fold func acc t) (func acc x) tail

let averageValInTree (tree: MyTree<int>) =
    (float <| MyTree.fold (fun acc x -> acc + x) 0 tree) / (float <| MyTree.fold (fun acc _ -> acc + 1) 0 tree)

let maxInTree (tree: MyTree<int>) =
    MyTree.fold (fun acc x -> if acc > x then acc else x) System.Int32.MinValue tree
