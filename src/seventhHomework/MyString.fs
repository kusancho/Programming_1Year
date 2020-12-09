module MyString

open System
open MyList

type MyString = MyList<char>

let concatMyStr (fst: MyString) (snd: MyString) =
    (MyList.Concat fst snd): MyString

let strToMyStr (str: string) =
    let lst = [for i in str -> i]
    (MyList.ofList lst): MyString

let myStrToStr (myLst: MyString) =
    let lst = MyList.toList (myLst.Map string)
    String.init lst.Length (fun i -> List.item i lst)
