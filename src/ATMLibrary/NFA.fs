module NFA

open System.Collections.Generic

type NFASmb<'t> =
    | Eps
    | Smb of 't

[<Struct>]
type NFA<'t> =
    val StartState : int
    val FinalState : int
    val Transitions : list<int * NFASmb<'t> * int>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let recognizeNFA (atm: NFA<_>) (input: list<_>) =
    let visited = HashSet<_>()
    let step curState curInput =
        visited.Add((curState, curInput)) |> ignore
        atm.Transitions
        |> List.choose (fun (s,t,f) ->
            if s = curState
            then
                match t with
                | Eps -> Some (f, curInput)
                | Smb smb ->
                    match curInput with
                    | curSmb :: restInput when smb = curSmb -> Some (f, restInput)
                    | _ -> None
            else None)

    let rec _go configurations =
        match configurations with
        | [] -> false
        | (s, input) :: tl ->
            let containsFinal =
                List.exists
                    (fun (s, input) -> s = atm.FinalState && input = [])
                    configurations
            let notVisited =
                step s input |> List.filter (fun x -> visited.Contains x |> not)
            containsFinal || (_go (tl @ notVisited))

    _go [(atm.StartState, input)]


let nfaToDot outFile (nfa:NFA<'t>) =
    let header =
        [
            "digraph nfa"
            "{"
            "rankdir = LR"
            "node [shape = circle];"
            sprintf "%A[shape = circle, label = \"%A_Start\"]" nfa.StartState nfa.StartState
        ]

    let footer =
        [
            sprintf "%A[shape = doublecircle]" nfa.FinalState
            "}"
        ]

    let content =
        [
            for (s,t,f) in nfa.Transitions ->
                sprintf
                    "%A -> %A [label = \"%s\"]"
                    s
                    f
                    (match t with Eps -> "Eps" | Smb t -> sprintf "%A" t)
        ]

    System.IO.File.WriteAllLines (outFile, header @ content @ footer)
