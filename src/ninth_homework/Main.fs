namespace homework

open AutomataOLD
open Regexp

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        (*let transitions = [(0,'0',1); (1,'1',1) ; (1,'0',1)]
        let atm = new DFA<_>(0, [1], transitions)
        recognizeDFA atm ['0'; '1'; '2'; '0'; '1']
        |> printfn "Recognition with DFA result: %A"  *)
        let massiv = [| |]
        let nfa =
            Seq(Star(RSmb '2'), RSmb '0') // 0(eps)*
            |> regexpToNFA

        nfaToDot "/home/kusancho/progahw/homework/src/ninth_homework/9dot" nfa

        recognizeNFA nfa ['2'; '2'; '0']
        |> printfn "Recognition with NFA result: %A"

        (*let nfa2 = // 1|2|3|4(1|2|3|4|0)*
            Seq(
                Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'),
                Star (Alt (Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'), RSmb '0'))
            )
            |> regexpToNFA

        nfaToDot "NFA2.dot" nfa2

        recognizeNFA nfa2 ['1']
        |> printfn "Recognition with NFA result: %A" *)


        0


