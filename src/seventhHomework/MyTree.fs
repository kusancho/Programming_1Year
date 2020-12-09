module MyTree

open MyList

type MyTree<'T> =
    | Node of 'T * MyList<MyTree<'T>>
    | Leaf of 'T

let rec foldTree func acc (myTree: MyTree<'T>) =
    match myTree with
    | Leaf value -> (func acc value)
    | Node (fst, myLst) -> func (MyList.Fold func acc (myLst.Map (fun x -> foldTree func acc x))) fst


//let sumOfIntTree (tree: MyTree<'T>) =
   // foldTree (fun acc x -> acc + x) 0 tree

//let countElemInMyTree (tree: MyTree<_>) =
   // foldTree (fun acc _ -> acc + 1) 0 tree

//let averageValueInMyTree (tree: MyTree<_>) =
   // (float (sumOfIntTree tree))/(float (countElemInMyTree tree))
