namespace seventhHomework

open MyList
open MyString

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let str = Cons('t',Cons('r',Last 'a'))
        printf "%A" <| myStrToStr str
        0
