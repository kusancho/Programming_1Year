module MyString

open System
open MyList

type MyString = MyList<char>

let concatMyStr (fst:MyList<char>) (snd: MyList<char>) =
    MyList.Concat fst snd

let strToMyStr (str: string) =
    let lst = [for i in str -> i]
    (MyList.ofList lst): MyString

let myStrToStr (myLst: MyList<char>) =
    let lst = MyList.toList (myLst.Map string)
    String.init lst.Length (fun i -> List.item i lst)
