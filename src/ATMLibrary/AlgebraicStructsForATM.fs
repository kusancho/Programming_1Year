module AlgebraicStructsForATM


open AlgebraicStructure
open Regexp
open System.Collections.Generic


let inline addSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
        Set.union s1 s2

let inline multSets (s1: Set<NFASmb<'t>>) (s2: Set<NFASmb<'t>>) =
    let temp = Set.map (fun s1 -> if s1 = Eps then s2 else Set.empty) s1
    Set.unionMany temp


let addHashSets (fst: HashSet<_>) snd =
    fst.UnionWith snd
    fst

let multHashSets (fst: HashSet<_>) snd =
    fst.IntersectWith snd
    fst


let algStrForBoolOp = SemiRing(new SemiRing<_>(new Monoid<_>((||), false), (&&)))

let algStrForSetsOp<'t when 't: comparison> =
    SemiRing(new SemiRing<Set<NFASmb<'t>>>(
                new Monoid<Set<NFASmb<'t>>>(addSets, Set.empty<NFASmb<'t>>), multSets))

let algStrForHashSetsOp<'t when 't: comparison> =
    SemiRing(new SemiRing<HashSet<NFASmb<'t>>>(
                new Monoid<HashSet<NFASmb<'t>>>(addHashSets, HashSet<NFASmb<'t>>()), multHashSets)
    )
