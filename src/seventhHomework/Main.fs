namespace seventhHomework

open MyList
open MyString

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let str = Cons(9,Cons(8,Last 4))
        printf "%A" <| MyList.Fold (fun acc elem -> elem + acc ) 0 str
        0
