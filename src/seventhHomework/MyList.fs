module MyList

type MyList<'T> =
    | Last of 'T
    | Cons of 'T * MyList<'T>

    static member fold func acc (myLst: MyList<'T>) =
        match myLst with
        | Last value -> func acc value
        | Cons(fst, snd) -> MyList.fold func (func acc fst) snd

    member this.Length =
        MyList.fold (fun acc _ -> acc + 1) 0 this

    member this.Head =
        match this with
        | Last value -> value
        | Cons(value, _) -> value

    member this.Tail =
        match this with
        | Last _ -> failwith "tail doesn't exist"
        | Cons(_, tl) -> tl

    static member rev (myLst: MyList<'T>) =
        if myLst.Length > 1
        then
            let rec reversing acc lst =
                match lst with
                | Last value -> Cons(value, acc)
                | Cons (fst, snd) -> reversing (Cons(fst, acc)) snd
            reversing (Last myLst.Head) myLst.Tail
        else myLst

    static member map func (myLst: MyList<'T>) =
        match myLst with
        | Last value -> Last (func value)
        | Cons(fst, snd) -> Cons(func fst, MyList.map func snd )

    static member iter func (myLst: MyList<'T>) =
        match myLst with
        | Last value -> func value
        | Cons(hd, tl) ->
            func hd
            MyList.iter func tl

    static member ofList (lst: list<'T>) =
        match lst.Length with
        | 0 -> failwith "myList is nonEmpty list"
        | 1 -> Last lst.Head
        | _ -> List.fold (fun acc elem -> (MyList.concat acc (Last elem))) (Last lst.Head) lst.Tail

    static member toList (myLst: MyList<'T>) =
        MyList.fold (fun acc elem -> acc @ [elem]) [] myLst

    static member concat (fst: MyList<'T>) (snd: MyList<'T>) =
        match fst with
            | Last value -> Cons(value, snd)
            | Cons(head, tail) -> Cons(head, MyList.concat tail snd)

let sortMyList (myLst: MyList<'T>) =
    let rec maxToTop myLst =
        match myLst with
        | Last value -> Last value
        | Cons(fst, Last snd) when fst > snd -> Cons(snd, Last fst)
        | Cons(fst, Last snd) -> Cons(fst, Last snd)
        | Cons(fst, Cons(snd, thd)) when fst > snd -> Cons(snd, maxToTop (Cons(fst, thd)))
        | Cons(fst, Cons(snd, thd)) -> Cons(fst, maxToTop (Cons(snd,thd)))

    let rec cicle (iter: int) (myLst: MyList<'T>) =
        match iter with
        | iter when iter = myLst.Length -> myLst
        | _ -> cicle (iter + 1) (maxToTop myLst)

    if myLst.Length = 1
    then myLst
    else cicle 0 myLst
