module ATM


open System.Collections.Generic
open Interface
open AlgebraicStructure
open Regexp


[<Struct>]
type NFA<'t when 't: comparison> =
    val StartState : HashSet<int>
    val FinalState : HashSet<int>
    val Transitions : IMatrix<Set<'t>>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let epsClosure (nfa: NFA<_>) algStr algStrForBoolOp
               (matrixBuilder: int -> int -> (int -> int -> Set<_>) -> IMatrix<_>) =

    let eCls = nfa.Transitions.transitiveClosure algStr

    let newFinals = HashSet()

    eCls.iteri
        (fun i j (item: Set<_>) -> if item.Contains Eps && nfa.FinalState.Contains j then newFinals.Add i |> ignore)

    newFinals.UnionWith nfa.FinalState

    let resTree = eCls.map (fun (set: Set<_>) -> set.Remove Eps)

    let reachable = (resTree.toBool Set.empty).transitiveClosure algStrForBoolOp

    let reachableFromStart = HashSet()

    reachable.iteri (fun i j _ -> if nfa.StartState.Contains i then reachableFromStart.Add j |> ignore)

    reachableFromStart.UnionWith nfa.StartState

    let newStateToOldState = Dictionary()

    reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (i, x)) // (new, old)

    NFA(
        newStateToOldState |> Seq.filter (fun x -> nfa.StartState.Contains x.Value)
        |> Seq.map (fun kvp -> kvp.Key)
        |> HashSet
        , newStateToOldState
          |> Seq.filter (fun x -> newFinals.Contains x.Value)
          |> Seq.map (fun kvp -> kvp.Key)
          |> HashSet
        , matrixBuilder newStateToOldState.Count newStateToOldState.Count
            (fun i j -> resTree.get(newStateToOldState.[i], newStateToOldState.[j], algStr)))


let intersect (fst: NFA<_>) (snd: NFA<_>) algStr algStrForBoolOp matrixBuilder =
    let monoid =
        match algStr with
        | SemiRing x -> x.Monoid
        | _ -> failwith "cannot multiply in monoid"

    let func (s1: Set<_>) (s2: Set<_>) =
        let s1Hash, s2Hash = HashSet(s1), HashSet(s2)
        let res = HashSet<_>(s1Hash) in res.IntersectWith s2Hash
        Set(res)

    let tempRing = SemiRing(new SemiRing<_>(monoid, func))

    let fstCls = epsClosure fst algStr algStrForBoolOp matrixBuilder

    let sndCls = epsClosure snd algStr algStrForBoolOp matrixBuilder

    let product = fstCls.Transitions.tensorMultiply sndCls.Transitions tempRing

    let newStartState =
        [ for s1 in fstCls.StartState do
              for s2 in sndCls.StartState do
                s1 * sndCls.Transitions.lineSize + s2
        ]
        |> HashSet

    let newFinalStates =
        [
            for s1 in fstCls.FinalState do
                for s2 in sndCls.FinalState do
                    s1 * sndCls.Transitions.lineSize + s2
        ]
        |> HashSet

    NFA(newStartState, newFinalStates, product)


let toDot (this: NFA<_>) outFile =

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
        |> fun a ->  a.fold (fun acc elem -> acc @ (Seq.toList elem)) []

    System.IO.File.WriteAllLines(outFile, header @ content @ footer)


let regexpToNFA (regexp: Regexp<'t>) (algStr: AlgebraicStruct<_>)
    (mtxBuilder: int -> int -> (int -> int -> HashSet<NFASmb<'t>>) -> IMatrix<HashSet<NFASmb<'t>>>) =
    let atm = regexpToListNFA regexp
    let mtx = mtxBuilder (atm.FinalState + 1) (atm.FinalState + 1) (fun _ _ -> HashSet())
    List.iter (fun elem -> let i, elem, j = elem
                           mtx.get(i, j, algStr).Add elem |> ignore) atm.Transitions
    NFA(HashSet([atm.StartState]), HashSet([atm.FinalState]), mtx.map Set)


let seqToNFA (input: list<_>) matrixBuilder =
    let tree = matrixBuilder  (input.Length + 1)
                              (input.Length + 1)
                              (fun i j -> if i + 1 = j then Set([Smb input.[i]]) else Set.empty<_>)
    NFA(HashSet([0]), HashSet([input.Length]), tree)


let accept (nfa: NFA<_>) (input: list<_>) algStr algStrForBoolOp matrixBuilder =

    let nfaStr = seqToNFA input matrixBuilder

    let intersection = intersect nfa nfaStr algStr algStrForBoolOp matrixBuilder

    let projected = intersection.Transitions.toBool Set.empty

    let reachability = projected.transitiveClosure algStrForBoolOp

    List.ofSeq intersection.FinalState
    |> List.fold (
        fun a s -> a || (Seq.fold (fun a2 s2 -> a2 || reachability.get(s2, s, algStrForBoolOp)) false intersection.StartState) ) false
