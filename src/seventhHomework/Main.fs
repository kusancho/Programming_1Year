namespace seventhHomework

open System
open MyList
open MyString
open MyTree

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let tree = (Node(8, Last (Node (2, Last (Leaf 2)))))

        0
