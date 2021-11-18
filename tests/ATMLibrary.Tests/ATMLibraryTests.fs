module ATMLibraryTests


open Expecto
open ATM
open Interface
open QuadTree
open Regexp
open MatrixBuilder
open AlgebraicStructsForATM
open System.Collections.Generic


let acceptForTests atm str =
    accept atm str algStrForSetsOp algStrForBoolOp (matrixBuilder QuadTree)


let getTransitionsFromNFA (nfa: NFA<_>) =
    let mutable acc = []
    nfa.Transitions.iteri (fun i j elem -> if elem <> Set.empty then acc <- acc @ [(i, j, elem)])
    acc


let fstAtm = regexpToNFA  (Star (Alt (RSmb '1', RSmb '0'))) algStrForHashSetsOp (matrixBuilder QuadTree) //('0', '1')*
let sndAtm = regexpToNFA (Seq (Star (Alt (RSmb '1', RSmb '0')), RSmb '3')) algStrForHashSetsOp (matrixBuilder QuadTree) // ('0', '1')* + '3'
let binStr1 = ['1'; '0'; '0'; '0'] // not acceptable for sndAtm
let binStr2 = ['1'; '0'; '1'; '3'] // acceptable for both
let emptyStr = []
let handMadeAtm = NFA(HashSet([0]), HashSet([2]), extendedTree(4, 4, Node(Node(None, Leaf <| Set([Smb 'a']), None, None), Node(None, None, Leaf <| Set([Smb 'b']), None), None, None)) :> IMatrix<_>)
let strAtm = seqToNFA ['a'; 'b'] (matrixBuilder QuadTree)


[<Tests>]
let testTree =
    testList "ATMs functions" [

        testCase "string ab to NFA - start state" <| fun _ ->
            Expect.equal (List.ofSeq strAtm.StartState) (List.ofSeq handMadeAtm.StartState) ""


        testCase "string ab to NFA - final state" <| fun _ ->
            Expect.equal (List.ofSeq strAtm.FinalState) (List.ofSeq handMadeAtm.FinalState) ""


        testCase "string ab to NFA - transitions" <| fun _ ->
            let transStr = getTransitionsFromNFA strAtm
            let transHandMade = getTransitionsFromNFA handMadeAtm
            Expect.equal transHandMade transStr ""


        testCase "accept 1000 by (0, 1)*" <| fun _ ->
            Expect.isTrue (acceptForTests fstAtm binStr1) ""


        testCase "accept 1000 by (0, 1)* + 3" <| fun _ ->
            Expect.isFalse (acceptForTests sndAtm binStr1) ""


        testCase "accept 1013 by (0, 1)*" <| fun _ ->
            Expect.isFalse (acceptForTests fstAtm binStr2) ""


        testCase "accept 1013 by (0, 1)* + 3" <| fun _ ->
            Expect.isTrue (acceptForTests sndAtm binStr2) ""


        testCase "accept empty string by (0, 1)*" <| fun _ ->
            Expect.isFalse (acceptForTests fstAtm emptyStr) ""


        testCase "accept empty string by (0, 1)* + 3" <| fun _ ->
            Expect.isFalse (acceptForTests sndAtm emptyStr) ""


        testCase "accept ab by atm of ab" <| fun _ ->
            Expect.isTrue (acceptForTests strAtm ['a'; 'b']) ""


        testCase "accept aba by atm of ab" <| fun _ ->
            Expect.isFalse (acceptForTests strAtm ['a'; 'b'; 'a']) ""


        testCase "accept ab by hand made atm" <| fun _ ->
            Expect.isTrue (acceptForTests handMadeAtm ['a'; 'b']) ""


        testCase "accept aba by hand made atm" <| fun _ ->
            Expect.isFalse (acceptForTests handMadeAtm ['a'; 'b'; 'a']) ""

]
