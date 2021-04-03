namespace homework


open Regexp
open TreeNFA


module Main =

    [<EntryPoint>]
    let main (argv: string array) =

        let nfa = Star(Alt(RSmb '1', RSmb '0')) |> regexpToNFA

        let eps = epsClosure <| nfaToTreeNFA nfa

        printfn "%A"  <| accept eps ['1'; '0'; '9']

        0


