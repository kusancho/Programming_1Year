module Regexp


type Regexp<'t> =
    | REps
    | RSmb of 't
    | Seq of Regexp<'t> * Regexp<'t>
    | Alt of Regexp<'t> * Regexp<'t>
    | Star of Regexp<'t>


type NFASmb<'t> =
    | Eps
    | Smb of 't


[<Struct>]
type ListNFA<'t> =
    val StartState : int
    val FinalState : int
    val Transitions : list<int * NFASmb<'t> * int>
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let regexpToListNFA regexp =
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
            ListNFA(newStart, newFinal, transitions)

    _go 0 regexp

