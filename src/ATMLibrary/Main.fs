namespace homework

open AutomataOLD
open Regexp
open Automata
open AlgebraicStructure
open System.Collections.Generic
open MatrixNFA

module Main =

    [<EntryPoint>]
    let main (argv: string array) =

        let nfa = Star(Alt(RSmb '1', RSmb '0')) |> regexpToNFA
        let inline addSets (s1:HashSet<_>) s2 =
            let r = if s1 = null then new HashSet<_>() else new HashSet<_>(s1)
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

        let semiRing = SemiRing(new SemiRing<_>(new Monoid<_>(addSets, null), multSets))

        nfaToDot "/home/kusancho/progahw/homework/src/ATMLibrary/NFA2.dot" nfa

        let eps = epsClosure <| nfaToTreeNFA nfa semiRing
        printf "%A"  <| accept eps ['1'; '0'; '1'] semiRing


        0


