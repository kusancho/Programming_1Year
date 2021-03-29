module Automata


open System.Collections.Generic
open AutomataOLD
open QuadTree
open SparseMatrix
open AlgebraicStructure
open Matrices


[<Struct>]
type TreeNFA<'t> =
    val StartState : HashSet<int>
    val FinalState : HashSet<int>
    val Transitions : extendedTree<HashSet<NFASmb<'t>>>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let algStrForBoolOp = SemiRing(new SemiRing<_>(new Monoid<_>((||), false), (&&)))


let array2DToTreeHash arr algStr =
    let acc = HashSet()
    Array2D.iteri (fun i j (item: HashSet<_>) -> if item.Count <> 0 then acc.Add <| Cell(i, j, item) |> ignore else ()) arr
    extendedTree.createTreeOfSparseMatrix algStr <| SparseMatrix(Array2D.length1 arr, Array2D.length2 arr, List.ofSeq acc)


let array2DToTreeBool arr =
    let tempAcc = HashSet()
    Array2D.iteri (fun i j k -> if k <> false then tempAcc.Add <| Cell(i, j, k) |> ignore else ()) arr
    extendedTree.createTreeOfSparseMatrix
        algStrForBoolOp
        <| SparseMatrix(Array2D.length1 arr, Array2D.length2 arr, List.ofSeq tempAcc)


let treeToArray2DBool tree =
    let lst = SparseMatrix.toListOfCells <| extendedTree.toSparseMatrix tree
    let mtx = Array2D.init tree.lineSize tree.colSize (fun _ _ -> false)
    List.iter (fun (i, j, item) -> if item then () else mtx.[i, j] <- item) lst
    mtx


let treeToArray2DHash (tree: extendedTree<_>) =
    let lst = SparseMatrix.toListOfCells <| extendedTree.toSparseMatrix tree
    let mtx = Array2D.init tree.lineSize tree.colSize (fun _ _ -> HashSet())
    List.iter (fun (i, j, item) -> mtx.[i, j] <- item) lst
    mtx


let nfaToTreeNFA (nfa: NFA<_>) algStruct =
    let tree =
        let maxState =
           nfa.Transitions
           |> List.fold (fun a (s, _, f) -> max (max s a) f) 0
        let mtx =
            Array2D.init
                (maxState + 1)
                (maxState + 1)
                (fun _ _ -> HashSet())

        nfa.Transitions
        |> List.iter (fun (s, l, f) -> mtx.[s, f].Add l |> ignore)
        array2DToTreeHash mtx algStruct
    TreeNFA(HashSet([nfa.StartState]), HashSet([nfa.FinalState]), tree)


let seqToAtm (input: list<_>) algStruct =
    let mtx =
        let mtx = Array2D.init (input.Length + 1) (input.Length + 1) (fun _ _ -> HashSet())
        for i in 0 .. input.Length - 1 do
            mtx.[i, i + 1].Add (Smb (input.[i])) |> ignore
        mtx
    TreeNFA(HashSet([0]), HashSet([input.Length]), array2DToTreeHash mtx algStruct)


let toDot (nfa: TreeNFA<_>) outFile =
    let header =
        [
            "digraph nfa"
            "{"
            "rankdir = LR"
            "node [shape = circle];"
            for s in nfa.StartState do
                sprintf "%A[shape = circle, label = \"%A_Start\"]" s s
        ]

    let footer =
        [
             for s in nfa.FinalState do
                sprintf "%A[shape = doublecircle]" s
             "}"
        ]

    let content =
        treeToArray2DHash nfa.Transitions
        |> Array2D.mapi (
            fun s f t ->
                t
                |> Seq.map (fun t ->
                    sprintf
                        "%A -> %A [label = \"%s\"]"
                        s
                        f
                        (match t with Eps -> "Eps" | Smb t -> sprintf "%A" t)))

        |> fun a ->  a |> Seq.cast<_> |> Seq.collect id |> Seq.toList

    System.IO.File.WriteAllLines (outFile, header @ content @ footer)


let epsClosure (atm: TreeNFA<_>) =
    let inline count (r: HashSet<_>[,]) =
        let mutable cnt = 0
        r |> Array2D.iter (fun i -> if i.Count > 0 then cnt <- cnt + 1)
        cnt

    let inline addSets (s1: HashSet<_>) s2 =
        let r = if s1 = null then HashSet<_>() else HashSet<_>(s1)
        r.UnionWith s2
        r

    let inline multSets s1 s2 =
        let r = new HashSet<_> ()
        for x in s1 do
            for y in s2 do
                match x,y with
                | Eps, e -> r.Add e |> ignore
                | _ -> ()
        r

    let monoid = Monoid(new Monoid<_>(addSets, null))
    let semiRing = SemiRing(new SemiRing<_>(new Monoid<_>(addSets, null), multSets))
    let eCls = extendedTree.transitiveClosure atm.Transitions semiRing
    let intermediateResult = TreeNFA<_> (atm.StartState, atm.FinalState, eCls)
    toDot intermediateResult "/home/kusancho/progahw/homework/src/ATMLibrary/eClsStep1.dot"


    let newFinals = new HashSet<_>()
    let eCls2D = treeToArray2DHash eCls
    eCls2D |> Array2D.iteri (fun i j x -> if x.Contains Eps && atm.FinalState.Contains j then newFinals.Add i |> ignore)
    newFinals.UnionWith atm.FinalState
    eCls2D |> Array2D.iteri (fun i j x -> x.Remove Eps |> ignore)
    let res = TreeNFA<_> (atm.StartState, newFinals, array2DToTreeHash eCls2D monoid)
    toDot res "/home/kusancho/progahw/homework/src/ATMLibrary/eClsWithoutEpsEdges.dot"


    let boolMtx = treeToArray2DHash res.Transitions |> Array2D.map (fun x -> x.Count > 0)
    let boolTree = array2DToTreeBool boolMtx

    let reachable = extendedTree.transitiveClosure boolTree algStrForBoolOp
    let reachableFromStart = HashSet<_>()
    for item in (extendedTree.toSparseMatrix reachable).content do
        if atm.StartState.Contains item.line then reachableFromStart.Add item.col |> ignore
    reachableFromStart.UnionWith atm.StartState
    let newStateToOldState = Dictionary<_,_>()
    reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (i,x))

    let newTransitions =
        let localVar = treeToArray2DHash res.Transitions
        let temp = Array2D.init
                    newStateToOldState.Count
                    newStateToOldState.Count
                    (fun i j -> localVar.[newStateToOldState.[i], newStateToOldState.[j]])
        array2DToTreeHash temp monoid

    let res =
        TreeNFA<_>(
            newStateToOldState |> Seq.filter (fun x -> atm.StartState.Contains x.Value)
            |> Seq.map (fun kvp -> kvp.Key)
            |> fun s -> HashSet(s)
            , newStateToOldState
              |> Seq.filter (fun x -> newFinals.Contains x.Value)
              |> Seq.map (fun kvp -> kvp.Key)
              |> fun x -> HashSet(x)
            , newTransitions)

    toDot res "/home/kusancho/progahw/homework/src/ATMLibrary/eClsFinalResult.dot"

    res


let accept (nfaTree: TreeNFA<_>) (input: list<_>) algStruct =
    let monoid, semiRing =
        match algStruct with
        | Monoid x -> failwith "cannot multiply in monoid"
        | SemiRing x -> x.Monoid, x
    let nfa2Tree = seqToAtm input algStruct
    let newRing = SemiRing(new SemiRing<_>(monoid, (fun s1 s2 -> let res = HashSet<_>(s1) in res.IntersectWith s2; res)))

    let intersection = extendedTree.tensorMultiply nfa2Tree.Transitions nfaTree.Transitions newRing
    let newStartState =
        [ for s1 in nfa2Tree.StartState do
              for s2 in nfaTree.StartState do
                s1 * (nfaTree.Transitions.lineSize) + s2
        ]
        |> fun s -> new HashSet<_>(s)

    let newFinalStates =
        [
            for s1 in nfa2Tree.FinalState do
                for s2 in nfaTree.FinalState do
                    s1 * (nfaTree.Transitions.lineSize) + s2
        ]

    toDot nfa2Tree "/home/kusancho/progahw/homework/src/ATMLibrary/nfa2.dot"
    toDot (TreeNFA<_>(newStartState, HashSet<_>(newFinalStates), intersection)) "/home/kusancho/progahw/homework/src/ATMLibrary/outIntersection.dot"

    let projectedArray = treeToArray2DHash intersection |> Array2D.map (fun s -> s.Count > 0)

    let reachabilityArr = treeToArray2DBool (extendedTree.transitiveClosure (array2DToTreeBool projectedArray) algStrForBoolOp)

    newFinalStates
    |> List.fold (
        fun a s -> a || (Seq.fold (fun a2 s2 -> a2 || reachabilityArr.[s2, s]) false newStartState)) false
