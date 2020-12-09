module MyList

type MyList<'T> =
    | Last of 'T
    | Cons of 'T * MyList<'T>

    member this.Length =
        let rec calculateLength acc lst =
            match lst with
            | Last _ -> (acc + 1)
            | Cons(_, tl) -> calculateLength (acc + 1) tl
        calculateLength 0 this

    member this.Rev =
        if this.Length > 1
        then
            let rec reversing acc lst =
                match lst with
                | Last value -> Cons(value, acc)
                | Cons (fst, snd) -> reversing (Cons(fst, acc)) snd
            reversing (Last this.Head) this.Tail
        else this

    member this.Head =
        match this with
        | Last value -> value
        | Cons (value, _) -> value

    member this.Tail =
        match this with
        | Last _ -> failwith "tail doesn't exist"
        | Cons (_, tl) -> tl

    member this.Map func =
        if this.Length > 1
        then
            let rec hidReal fstLst sndLst =
                match fstLst with
                | Last value -> Cons(func value, sndLst)
                | Cons (fst, snd) -> hidReal snd (Cons (func fst, sndLst))
            (hidReal this.Tail (Last <| func this.Head)).Rev
        else Last (func this.Head)

    member this.Iter func =
        let rec hidReal lst =
            match lst with
            | Last value -> func value
            | Cons (fst, snd) ->
                func fst
                hidReal snd
        hidReal this

    static member Fold func acc (myLst: MyList<'T>) =
        match myLst with
        | Last value -> func acc value
        | Cons(fst, snd) -> MyList.Fold func (func acc fst) snd

    static member ofList (lst: list<'T>) =
        match lst.Length with
        | 0 -> failwith "myList is nonEmpty list"
        | 1 -> (Last lst.Head)
        | _ ->
            let rec hidReal (myLst: MyList<'T>) (lst: list<'T>) =
                match lst with
                | [] -> failwith "myList is nonEmpty list"
                | [a] -> Cons(a, myLst)
                | hd :: tl -> hidReal (Cons(hd, myLst)) tl
            hidReal (Last (List.rev lst).Head) (List.rev lst).Tail

    static member toList (myLst: MyList<'T>) =
        let rec hidReal (acc: list<'T>) (ost: MyList<'T>) =
            match ost with
            | Last value -> acc @ [value]
            | Cons(value, myLst) -> hidReal (acc @ [value]) myLst
        hidReal [] myLst

    static member Concat (fst: MyList<'T>) (snd: MyList<'T>) =
        let rec hidReal fstLst sndLst =
            match fstLst with
            | Last value -> Cons(value, sndLst)
            | Cons (hd, tl) -> hidReal tl (Cons(hd, sndLst))
        hidReal fst.Rev snd

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
