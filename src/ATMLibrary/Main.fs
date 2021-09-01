namespace homework


open QuadTree

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let qtree = Node(Leaf 1, Leaf 2, Leaf 3, Leaf 4)
        let tree = extendedTree(2, 2, qtree)
        extendedTree.iteri (fun i j elem -> printf "%A %A %A \n" i j elem) tree
        0


