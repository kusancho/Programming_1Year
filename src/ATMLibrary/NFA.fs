module NFA


open advancedMatrix


type ISmb<'t> =
    abstract PrettyString: string


type Smb<'t> =
    | Eps
    | Smb of 't
    interface ISmb<'t> with
        member this.PrettyString =
            match this with
            | Eps -> "Eps"
            | Smb t -> t.ToString()


type NFA<'t> =
    val StartState: int
    val FinalState: int
    val TransitionsList : advancedMatrix<int * int * ISmb<'t>>
    member this.ToDot outFile =
        let header =
            [
                "digraph nfa"
                "{"
                "rankdir = LR"
                "node [shape = circle];"
                sprintf "%A[shape = circle, label = \"%A_Start\"]" this.StartState this.StartState
            ]

        let footer =
            [
                sprintf "%A[shape = doublecircle]" this.FinalState
                "}"
            ]

        let content =
            [
                for s, f, t in this.TransitionsList do
                    sprintf
                        "%A -> %A [label = \"%s\"]"
                        s
                        f
                        t.PrettyString
            ]

        System.IO.File.WriteAllLines (outFile, header @ content @ footer)

    member this.epsClosure =

        let eCls = this.transitiveClosure

        let newFinals = HashSet<_>()

        extendedTree.iteri
            (fun i j (item: Set<_>) -> if item.Contains Eps && atm.FinalState.Contains j then newFinals.Add i |> ignore)
            eCls

        newFinals.UnionWith atm.FinalState

        let resTree = extendedTree.map (fun (set: Set<_>) -> set.Remove Eps) eCls

        let boolTree = extendedTree.toBoolTree resTree

        let reachable = extendedTree.transitiveClosure boolTree algStrForBoolOp

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
