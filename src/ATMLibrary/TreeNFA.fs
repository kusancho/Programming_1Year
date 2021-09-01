module TreeNFA


open System.Collections.Generic
open ListNFA
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


let nfaToTreeNFA (nfa: ListNFA<_>) =
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


let TreeNFAOfRegExp regexp = nfaToTreeNFA <| regexpToListNFA regexp


let seqToAtm (input: list<_>) =
    let tree = extendedTree.init (input.Length + 1) (input.Length + 1)
                                 (fun i j -> if i + 1 = j then Set([Smb input.[i]]) else Set.empty<_>)
               |> extendedTree.clearNeutral Set.empty<_>
    TreeNFA(HashSet([0]), HashSet([input.Length]), tree)


let toDot (nfaTree: TreeNFA<'t>) outFile =
    let header =
        [
            "digraph nfa"
            "{"
            "rankdir = LR"
            "node [shape = circle];"
            for s in nfaTree.StartState do
                sprintf "%A[shape = circle, label = \"%A_Start\"]" s s
        ]

    let footer =
        [
             for s in nfaTree.FinalState do
                sprintf "%A[shape = doublecircle]" s
             "}"
        ]

    let content =
        extendedTree.mapi (fun i j item -> Cell(i, j, item)) nfaTree.Transitions
        |> extendedTree.fold (fun acc item -> acc @ [(item.line, item.col, item.data)]) []
        |> List.map (
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

    let eCls = atm.Transitions.transitiveClosure algStrForSetsOp

    let newFinals = HashSet<_>()

    extendedTree.iteri
        (fun i j (item: Set<_>) -> if item.Contains Eps && atm.FinalState.Contains j then newFinals.Add i |> ignore)
        eCls

    newFinals.UnionWith atm.FinalState

    let resTree = extendedTree.map (fun (set: Set<_>) -> set.Remove Eps) eCls

    let boolTree = extendedTree.toBoolTree resTree

    let reachable = boolTree.transitiveClosure algStrForBoolOp

    let reachableFromStart = HashSet<_>()

    extendedTree.iteri (fun i j _ ->if atm.StartState.Contains i then reachableFromStart.Add j |> ignore) reachable

    reachableFromStart.UnionWith atm.StartState

    let newStateToOldState = Dictionary<_,_>()

    reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (i, x)) // (new, old)

    let tree = extendedTree.init newStateToOldState.Count newStateToOldState.Count
                   (fun _ _ -> Set.empty<_>)

    let newTransitions =
        let temp = resTree.fillNeutral Set.empty<_>
        tree
        |> extendedTree.mapi  (fun i j item -> temp.getByIndex newStateToOldState.[i] newStateToOldState.[j])

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

    let reachability = projected.transitiveClosure algStrForBoolOp

    let fullEcls = reachability.fillNeutral false

    newFinalStates
    |> List.fold (
        fun a s -> a || (Seq.fold (fun a2 s2 -> a2 || fullEcls.getByIndex s2 s) false newStartState) ) false
