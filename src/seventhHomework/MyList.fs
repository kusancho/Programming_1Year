module MyList

open System

type MyList<'T> =
    | Last of 'T
    | Cons of 'T * MyList<'T>

    member this.Length =
        let rec calculateLength acc lst =
            match lst with
            | Last _ -> (acc + 1)
            | Cons(_, tl) -> calculateLength (acc + 1) tl
        calculateLength 0 this

    member this.rev =
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

let makeMyListOfList (lst: list<'T>) =
    match lst.Length with
    | 0 -> failwith "myList is nonEmpty list"
    | 1 -> (Last lst.Head)
    | _ ->
        let rec hidReal (myLst: MyList<_>) (lst: list<_>) =
            match lst with
            | [] -> failwith "myList is nonEmpty list"
            | [a] -> Cons(a, myLst)
            | hd :: tl -> hidReal (Cons(hd, myLst)) tl
        hidReal (Last (List.rev lst).Head) (List.rev lst).Tail

let concatMyLists (fstLst: MyList<_>) (sndLst: MyList<_>) =
    let rec hidReal fstLst sndLst =
        match fstLst with
        | Last value -> Cons(value, sndLst)
        | Cons (hd, tl) -> hidReal tl (Cons(hd, sndLst))
    hidReal fstLst.rev sndLst

let makeListOfMyList (myLst: MyList<'T>) =
    let rec hidReal (acc: list<'T>) (ost: MyList<'T>) =
        match ost with
        | Last value -> acc @ [value]
        | Cons(value, myLst) -> hidReal (acc @ [value]) myLst
    hidReal [] myLst
