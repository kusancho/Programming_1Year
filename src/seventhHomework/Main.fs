namespace seventhHomework

open MyList

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let lst = [1;43]
        let lst2 = [123;5]
        //let myLst1 = makeMyListOfList lst
        //let myLst2 = makeMyListOfList lst2

        printf "%A" (makeListOfMyList <| makeMyListOfList lst)
        0
