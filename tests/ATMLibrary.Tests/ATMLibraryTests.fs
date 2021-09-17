module ATMLibraryTests

open Expecto
open ListNFA
open QuadTree
open TreeNFA
open Regexp
open SparseMatrix
open System.Collections.Generic
open ATM
open interfaces


let fstAtm = TreeNFAOfRegExp <| Star(Alt(RSmb '1', RSmb '0')) //('0', '1')*
let sndAtm = TreeNFAOfRegExp <| Seq(Star(Alt(RSmb '1', RSmb '0')), RSmb '3') // ('0', '1')* + '1'
let str = ['t'; 'e'; 's'; 't']
let binStr1 = ['1'; '0'; '0'; '0'] // not acceptable
let binStr2 = ['1'; '0'; '1'; '3'] // acceptable

[<Tests>]
let testTree =
    testList "ATMs functions" [

//        testCase "seq to ATM" <| fun _ ->
//            let atm = NFA<_>.seqToAtm str
//            let hs = HashSet<_>()
//            atm.Transitions.iteri (fun i j elem -> if elem = Set.empty then () else hs.Add((i, j,  elem)) |> ignore)
//            let origTrans = Set([(0, 1, Set([Smb 't'])); (1, 2, Set([Smb 'e']))
//                                 (2, 3, Set([Smb 's'])); (3, 4, Set([Smb 't']))])
//            Expect.equal (Set(hs)) origTrans " "


        testCase "NFA of ListNFA" <| fun _ ->
            let atm = TreeNFAOfRegExp <| Star(Alt(RSmb '1', RSmb '0'))
            Expect.equal (accept atm binStr1) true ""


        testCase "accept #1" <| fun _ ->
            Expect.equal true (accept fstAtm binStr1) " "


        testCase "accept #2" <| fun _ ->
            Expect.equal false (accept sndAtm binStr1) " "


        testCase "accept #3" <| fun _ ->
            Expect.equal true (accept sndAtm binStr2) " "

        testCase "accept #4" <| fun _ ->
            Expect.equal false (accept fstAtm binStr2) " "
            ]