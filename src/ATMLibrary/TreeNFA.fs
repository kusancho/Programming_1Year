module TreeNFA


open System.Collections.Generic
open NFA
open QuadTree
open Regexp
open AlgebraicStructure
open SparseMatrix


[<Struct>]
type TreeNFA<'t when 't: comparison> =
    val StartState : HashSet<int>
    val FinalState : HashSet<int>
    val Transitions : extendedTree<Set<NFASmb<'t>>>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let inline addSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
        Set.union s1 s2

let inline multSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
    let temp = Set.map (fun s1 -> if s1 = Eps then s2 else Set.empty) s1
    Set.unionMany temp


let algStrForBoolOp = SemiRing(new SemiRing<_>(new Monoid<_>((||), false), (&&)))

let algStrForSetsOp<'t when 't: comparison> = SemiRing(new SemiRing<Set<NFASmb<'t>>>(new Monoid<Set<NFASmb<'t>>>(addSets, Set.empty<NFASmb<'t>>), multSets))


let nfaToTreeNFA (nfa: NFA<_>) =
    let tree =
        let dict = Dictionary<_, HashSet<_>>()
        let maxState =
           nfa.Transitions
           |> List.fold (fun a (s, _, f) -> max (max s a) f) 0
        nfa.Transitions
        |> List.iter (fun (s, l, f) ->
            if dict.ContainsKey((s, f))
            then dict.[(s, f)].Add l |> ignore
            else
                dict.Add((s, f), HashSet())
                dict.[(s, f)].Add l |> ignore)
        let lst = [for keyValue in dict do
                       let i, j = keyValue.Key
                       Cell(i, j, Set(keyValue.Value))]
        extendedTree.createTreeOfSparseMatrix algStrForSetsOp <| SparseMatrix(maxState + 1, maxState + 1, lst)
    TreeNFA(HashSet([nfa.StartState]), HashSet([nfa.FinalState]), tree)


let TreeNFAOfRegExp regexp = nfaToTreeNFA <| regexpToNFA regexp


let seqToAtm (input: list<_>) =
    let rec makeLst lst index acc =
        match lst with
        | hd :: tl -> makeLst tl (index + 1) (acc @ [Cell(index, index + 1, Set([Smb hd]))])
        | [] -> acc
    let sparse = SparseMatrix(input.Length + 1, input.Length + 1, makeLst input 0 [])
    TreeNFA(HashSet([0]), HashSet([input.Length]), extendedTree.createTreeOfSparseMatrix algStrForSetsOp sparse)


let toDot (nfa: TreeNFA<'t>) outFile =
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
        let lst = SparseMatrix.toListOfCells (extendedTree.toSparseMatrix nfa.Transitions)
        lst |> List.map (
            fun (s, f, t) ->
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

    let eCls = extendedTree.transitiveClosure atm.Transitions algStrForSetsOp

    let newFinals = HashSet<_>()

    let cells = List.map (fun (cell: Cell<Set<_>>) ->
        Cell(cell.line, cell.col, HashSet(cell.data))) (extendedTree.toSparseMatrix eCls).content

    // newFinals filling
    List.iter (fun (cell: Cell<HashSet<_>>) ->
        if cell.data.Contains Eps && atm.FinalState.Contains cell.col then newFinals.Add cell.line |> ignore) cells

    newFinals.UnionWith atm.FinalState

    // removing eps edges
    List.iter (fun (cell: Cell<HashSet<_>>) -> cell.data.Remove Eps |> ignore ) cells

    let resTree = extendedTree.createTreeOfSparseMatrix algStrForSetsOp
                  <| SparseMatrix(eCls.lineSize, eCls.colSize,
                                  List.map (fun (c: Cell<HashSet<_>>) -> Cell(c.line, c.col, Set(c.data))) cells)

    let boolTree = extendedTree.toBoolTree resTree

    let reachable = extendedTree.transitiveClosure boolTree algStrForBoolOp

    let reachableFromStart = HashSet<_>()

    for item in (extendedTree.toSparseMatrix reachable).content do
        if atm.StartState.Contains item.line then reachableFromStart.Add item.col |> ignore

    reachableFromStart.UnionWith atm.StartState

    let newStateToOldState = Dictionary<_,_>()

    reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (x, i)) // (old, new)

    let cellsRes = (extendedTree.toSparseMatrix resTree).content // res -> reachable

    let newTransitions =
        let localLst = List.filter (fun (cell: Cell<_>) -> newStateToOldState.ContainsKey cell.line
                                                               && newStateToOldState.ContainsKey cell.line ) cellsRes

        let final = List.map (fun (cell: Cell<_>) -> Cell(newStateToOldState.[cell.line],
                                                          newStateToOldState.[cell.col], cell.data)) localLst
        SparseMatrix(newStateToOldState.Count, newStateToOldState.Count, final)
        |> extendedTree.createTreeOfSparseMatrix algStrForSetsOp

    let res =
        TreeNFA<_>(
            newStateToOldState |> Seq.filter (fun x -> atm.StartState.Contains x.Key)
            |> Seq.map (fun kvp -> kvp.Value)
            |> fun s -> HashSet(s)
            , newStateToOldState
              |> Seq.filter (fun x -> newFinals.Contains x.Key)
              |> Seq.map (fun kvp -> kvp.Value)
              |> fun x -> HashSet(x)
            , newTransitions)

    res


let intersect fst snd =
    let monoid =
        match algStrForSetsOp with
        | SemiRing x -> x.Monoid
        | _ -> failwith "cannot multiply in monoid"

    let func (s1: Set<_>) (s2: Set<_>) =
        let s1Hash, s2Hash = HashSet(s1), HashSet(s2)
        let res = HashSet<_>(s1Hash) in res.IntersectWith s2Hash
        Set(res)

    let tempRing = SemiRing(new SemiRing<_>(monoid, func))

    extendedTree.tensorMultiply snd fst tempRing


let accept (nfaTree: TreeNFA<_>) (input: list<_>) =
    let nfa2Tree = seqToAtm input

    let eCls = epsClosure nfaTree

    let intersection = intersect eCls.Transitions nfa2Tree.Transitions

    let newStartState =
        [ for s1 in nfa2Tree.StartState do
              for s2 in eCls.StartState do
                s1 * (eCls.Transitions.lineSize) + s2
        ]
        |> fun s -> HashSet<_>(s)

    let newFinalStates =
        [
            for s1 in nfa2Tree.FinalState do
                for s2 in eCls.FinalState do
                    s1 * eCls.Transitions.lineSize + s2
        ]

    let projected = extendedTree.toBoolTree intersection

    let reachability = (extendedTree.transitiveClosure projected algStrForBoolOp)

    let cells = (extendedTree.toSparseMatrix reachability).content

    let existsPath start final =
        List.fold (fun acc3 (cell: Cell<_>) -> acc3 || cell.line = start && cell.col = final) false cells

    newFinalStates
    |> List.fold (
        fun aсс final -> aсс || (Seq.fold (fun acc2 start -> acc2 || existsPath start final) false newStartState)) false
