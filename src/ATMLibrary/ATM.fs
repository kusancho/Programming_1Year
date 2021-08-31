module ATM


open System.Collections.Generic
open Matriсes
open interfaces
open AlgebraicStructure
open ListNFA
open QuadTree
open Regexp


type NFASmb<'t when 't: comparison> =
    | Eps
    | Smb of 't


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
    val Transitions : IMatrix<Set<NFASmb<'t>>>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


    static member listNfaToNFA (nfa: ListNFA<'a>) =
        let mtx =
            let maxState =
               nfa.Transitions
               |> List.fold (fun a (s,_,f) -> max (max s a) f) 0
            let mtx = QuadTreeMtx(extendedTree.init
                                      (maxState + 1)
                                      (maxState + 1)
                                      (fun _ _ -> HashSet<'a>()),
                                      algStrForSetsOp) :> IMatrix<_>
            //let mtx = Unchecked.defaultof<IMatrix<_>>
            nfa.Transitions
            |> List.iter (fun (s,l,f) -> ((mtx.get(s, f)).Add l) |> ignore)
            mtx.map (fun elem -> Set(elem))
        NFA<_>(HashSet<_>([nfa.StartState]), HashSet<_>([nfa.FinalState]), mtx)


    static member TreeNFAOfRegExp regexp =
        NFA.listNfaToNFA <| regexpToListNFA regexp


    static member seqToAtm (input: list<_>) =
        let tree = QuadTreeMtx(extendedTree.init (input.Length + 1) (input.Length + 1)
                                     (fun i j -> if i + 1 = j then Set([Smb input.[i]]) else Set.empty<_>),
                                     algStrForSetsOp) :> IMatrix<_>
        NFA(HashSet([0]), HashSet([input.Length]), tree)
