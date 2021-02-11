module LongInt

open System.IO
open MyList
open System

//quadro
let readMatrixFromFileToArrayRepresentation fileName =
    let strArr = File.ReadAllLines fileName
    [| for lineN in 0 .. strArr.Length - 1 do [|for i in strArr.[lineN] do int i  |]  |]

type quadTree<'T> =
    | Data of 'T
    | Node of MyList<quadTree<'T> * quadTree<'T> * quadTree<'T> * quadTree<'T>>

    (*let arr = Array2D.zeroCreate strArr.Length strArr.[0].Length
    for lineN in 0 .. strArr.Length - 1 do
        for rowN in 0 .. strArr.[0].Length - 1 do
            arr.[lineN].[rowN] <- int strArr.[lineN].[rowN] *)


(*member addZeroes size =
    let lst = [for _ in 1 .. size -> 0]
    MyList.concat this (MyList.ofList lst)


type BigInt =
    | Content of MyList<int>
    | Sign of bool

    static member sum (first: BigInt) (second: BigInt) =
        let rec hidReal flag (fst: MyList<int>) (snd: MyList<int>) (lst: list<int>) = // let fst be not less than snd
            if snd.Length <> 1
            then hidReal ((fst.Head + snd.Head) / 10) fst.Tail snd.Tail (lst @ [(fst.Head + snd.Head + flag) % 10])
            else
                if fst.Head + snd.Head + flag > 9
                then lst @ [(fst.Head + snd.Head + flag) % 10] @ [1]
                else lst @ [fst.Head + snd.Head + flag]
        match first with
        | Content firstLst ->
            match second with
            | Content secondLst when firstLst.Length = secondLst.Length -> hidReal 0 firstLst secondLst []
            | Content secondLst when firstLst.Length > secondLst.Length -> hidReal 0 firstLst (addZeros secondLst (firstLst.Length - secondLst.Length)) []
            | Content secondLst -> hidReal 0 secondLst (addZeros firstLst (secondLst.Length - firstLst.Length)) []
            | _ -> hidReal 0 (Cons(3, Last 4)) firstLst []
        | _ -> [3]*)










