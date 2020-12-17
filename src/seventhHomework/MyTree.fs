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
     let (x,y) = MyTree.fold (fun (fst, snd) x -> fst + x, snd + 1) (0, 0) tree
     float x / float y

let maxInTree (tree: MyTree<int>) =
    MyTree.fold (fun acc x -> if acc > x then acc else x) System.Int32.MinValue tree
