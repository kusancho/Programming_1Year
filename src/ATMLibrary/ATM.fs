module ATM


open System.Collections.Generic
open Matriсes
open QuadTree
open interfaces
open AlgebraicStructure
open ListNFA
open Regexp

let inline addSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
        Set.union s1 s2

let inline multSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
    let temp = Set.map (fun s1 -> if s1 = Eps then s2 else Set.empty) s1
    Set.unionMany temp


let algStrForBoolOp = SemiRing(new SemiRing<_>(new Monoid<_>((||), false), (&&)))

let algStrForSetsOp<'t when 't: comparison> = SemiRing(new SemiRing<Set<NFASmb<'t>>>(new Monoid<Set<NFASmb<'t>>>(addSets, Set.empty<NFASmb<'t>>), multSets))


[<Struct>]
type NFA<'t when 't: comparison> =
    val StartState : HashSet<int>
    val FinalState : HashSet<int>
    val Transitions : IMatrix<Set<NFASmb<'t>>, Set<NFASmb<'t>>>
    val AlgebraicStruct: AlgebraicStruct<Set<NFASmb<'t>>>
    new (start, final, transitions, algStruct) =
        {StartState = start; FinalState = final; Transitions = transitions; AlgebraicStruct = algStruct}


    static member toDot (this: NFA<_>) outFile =

        let header =
            [
                "digraph nfa"
                "{"
                "rankdir = LR"
                "node [shape = circle];"
                for s in this.StartState do
                    sprintf "%A[shape = circle, label = \"%A_Start\"]" s s
            ]

        let footer =
            [
                 for s in this.FinalState do
                    sprintf "%A[shape = doublecircle]" s
                 "}"
            ]

        let content =
            this.Transitions.mapi  (
                fun s f t ->
                    t
                    |> Set.map (fun t ->
                        sprintf
                            "%A -> %A [label = \"%s\"]"
                            s
                            f
                            (match t with Eps -> "Eps" | Smb t -> sprintf "%A" t)))
            |> fun a ->  a.fold (fun acc elem -> acc @ (Seq.toList <| Seq.cast<_> elem)) []

        System.IO.File.WriteAllLines (outFile, header @ content @ footer)


    static member regexpToNFA (regexp: Regexp<'t>) =
        let rec _go curFreeState curRegexp =
            match curRegexp with
            | REps -> ListNFA(curFreeState, curFreeState + 1,
                                    [ (curFreeState, Eps, curFreeState + 1) ])
            | RSmb s -> ListNFA(curFreeState, curFreeState + 1,
                                    [ (curFreeState, Smb(s), curFreeState + 1) ])
            | Alt (l, r) ->
                let lAtm = _go curFreeState l
                let rAtm = _go (lAtm.FinalState + 1) r
                let newStart = rAtm.FinalState + 1
                let newFinal = rAtm.FinalState + 2
                let transitions =
                    [
                        (newStart, Eps, lAtm.StartState)
                        (newStart, Eps, rAtm.StartState)
                        (lAtm.FinalState, Eps, newFinal)
                        (rAtm.FinalState, Eps, newFinal)
                    ]
                    @ rAtm.Transitions
                    @ lAtm.Transitions
                ListNFA(newStart, newFinal, transitions)

            | Seq (l, r) ->
                let lAtm = _go curFreeState l
                let rAtm = _go (lAtm.FinalState + 1) r
                let newStart = rAtm.FinalState + 1
                let newFinal = rAtm.FinalState + 2
                let transitions =
                    [
                        (newStart, Eps, lAtm.StartState)
                        (lAtm.FinalState, Eps, rAtm.StartState)
                        (rAtm.FinalState, Eps, newFinal)
                    ]
                    @ rAtm.Transitions
                    @ lAtm.Transitions
                ListNFA(newStart, newFinal, transitions)

            | Star r ->
                let newAtm = _go curFreeState r
                let newStart = newAtm.FinalState + 1
                let newFinal = newAtm.FinalState + 2
                let transitions =
                    [
                        (newStart, Eps, newAtm.StartState)
                        (newAtm.FinalState, Eps, newFinal)
                        (newStart, Eps, newFinal)
                        (newFinal, Eps, newStart)
                    ]
                    @ newAtm.Transitions
                ListNFA<_> (newStart, newFinal, transitions)

        let atm = _go 0 regexp
        let mtx = QuadTreeMtx(extendedTree.init (atm.FinalState + 1) (atm.FinalState + 1) (fun _ _ -> HashSet())) :> IMatrix<_, _>
        List.iter (fun elem -> let a, b, c = elem
                               mtx.get(a, c).Add b |> ignore) atm.Transitions
        NFA(HashSet(atm.StartState), HashSet(atm.FinalState), mtx.map Set, algStrForSetsOp)


    static member seqToAtm (input: list<_>) =
        let tree = QuadTreeMtx(extendedTree.init (input.Length + 1) (input.Length + 1)
                                     (fun i j -> if i + 1 = j then Set([Smb input.[i]]) else Set.empty<_>)) :> IMatrix<_, _>
        NFA(HashSet([0]), HashSet([input.Length]), tree, algStrForSetsOp)


    static member intersect (fst: IMatrix<_, _>) (snd: IMatrix<_, _>) =
        let monoid =
            match algStrForSetsOp with
            | SemiRing x -> x.Monoid
            | _ -> failwith "cannot multiply in monoid"

        let func (s1: Set<_>) (s2: Set<_>) =
            let s1Hash, s2Hash = HashSet(s1), HashSet(s2)
            let res = HashSet<_>(s1Hash) in res.IntersectWith s2Hash
            Set(res)

        let tempRing = SemiRing(new SemiRing<_>(monoid, func))

        snd.tensorMultiply fst tempRing


    static member epsClosure (this: NFA<_>) =

        let eCls = this.Transitions.transitiveClosure algStrForSetsOp

        let newFinals = HashSet<_>()

        eCls.iteri
            (fun i j (item: Set<_>) -> if item.Contains Eps && this.FinalState.Contains j then newFinals.Add i |> ignore)

        newFinals.UnionWith this.FinalState

        let resTree = eCls.map (fun (set: Set<_>) -> set.Remove Eps)

        let boolTree = resTree.toBool algStrForBoolOp

        let reachable = boolTree.transitiveClosure algStrForBoolOp

        let reachableFromStart = HashSet<_>()

        reachable.iteri (fun i j _ -> if this.StartState.Contains i then reachableFromStart.Add j |> ignore)

        reachableFromStart.UnionWith this.StartState

        let newStateToOldState = Dictionary<_, _>()

        reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (i, x)) // (new, old)

        let tree = QuadTreeMtx(extendedTree.init newStateToOldState.Count newStateToOldState.Count (fun _ _ -> Set.empty<_>)) :> IMatrix<_, _>

        let newTransitions =
            // fil neutral
            tree.mapi (fun i j _ -> resTree.get(newStateToOldState.[i], newStateToOldState.[j]))

        let res =
            NFA(
                newStateToOldState |> Seq.filter (fun x -> this.StartState.Contains x.Key)
                |> Seq.map (fun kvp -> kvp.Value)
                |> fun s -> HashSet(s)
                , newStateToOldState
                  |> Seq.filter (fun x -> newFinals.Contains x.Key)
                  |> Seq.map (fun kvp -> kvp.Value)
                  |> fun x -> HashSet(x)
                , newTransitions, algStrForSetsOp)
        res


    static member accept (nfa: NFA<_>) (input: list<_>) =
        let nfa2Tree = NFA<_>.seqToAtm input

        let eCls = NFA<_>.epsClosure nfa

        let intersection = NFA<_>.intersect eCls.Transitions nfa2Tree.Transitions

        let newStartState =
            [ for s1 in nfa2Tree.StartState do
                  for s2 in eCls.StartState do
                    s1 * eCls.Transitions.lineSize + s2
            ]
            |> fun s -> HashSet<_>(s)

        let newFinalStates =
            [
                for s1 in nfa2Tree.FinalState do
                    for s2 in eCls.FinalState do
                        s1 * eCls.Transitions.lineSize + s2
            ]

        let projected = intersection.toBool algStrForBoolOp

        let reachability = projected.transitiveClosure algStrForBoolOp

        //let fullEcls = reachability.fillNeutral false

        newFinalStates
        |> List.fold (
            fun a s -> a || (Seq.fold (fun a2 s2 -> a2 || reachability.get(s2, s)) false newStartState) ) false
