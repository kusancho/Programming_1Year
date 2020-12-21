module MyString

open MyList

type MyString = MyList<char>

let concatMyStr (fst: MyString) (snd: MyString) =
    (MyList.concat fst snd): MyString

let strToMyStr (str: string) =
    let lst = [for i in str -> i]
    (MyList.ofList lst): MyString

let myStrToStr (myLst: MyString) =
    let lst = MyList.toList (MyList.map string myLst)
    String.init lst.Length (fun i -> List.item i lst)
