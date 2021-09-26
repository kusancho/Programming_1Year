module ATMLibraryTests


open Expecto
open ATM
open Regexp
open MatrixBuilder


let fstAtm = regexpToNFA  (Star (Alt (RSmb '1', RSmb '0')))  (matrixBuilder QuadTree) //('0', '1')*
let sndAtm = regexpToNFA (Seq (Star (Alt (RSmb '1', RSmb '0')), RSmb '3')) (matrixBuilder QuadTree) // ('0', '1')* + '3'
let binStr1 = ['1'; '0'; '0'; '0'] // not acceptable for sndAtm
let binStr2 = ['1'; '0'; '1'; '3'] // acceptable for both


[<Tests>]
let testTree =
    testList "ATMs functions" [

        testCase "accept 1000 by (0, 1)*" <| fun _ ->
            Expect.equal (accept fstAtm binStr1 (matrixBuilder QuadTree)) true ""


        testCase "accept 1000 by (0, 1)* + 3" <| fun _ ->
            Expect.equal false (accept sndAtm binStr1 (matrixBuilder QuadTree)) " "


        testCase "accept 1013 by (0, 1)*" <| fun _ ->
            Expect.equal (accept fstAtm binStr2 (matrixBuilder QuadTree)) false ""


        testCase "accept 1013 by (0, 1)* + 3" <| fun _ ->
            Expect.equal (accept sndAtm binStr2 (matrixBuilder QuadTree)) true ""

]
