module ATMLibraryTests


open Expecto
open ATM
open Regexp
open MatrixBuilder
open AlgebraicStructsForATM


let acceptForTests atm str =
    accept atm str algStrForSetsOp algStrForBoolOp (matrixBuilder QuadTree)


let fstAtm = regexpToNFA  (Star (Alt (RSmb '1', RSmb '0'))) algStrForHashSetsOp (matrixBuilder QuadTree) //('0', '1')*
let sndAtm = regexpToNFA (Seq (Star (Alt (RSmb '1', RSmb '0')), RSmb '3')) algStrForHashSetsOp (matrixBuilder QuadTree) // ('0', '1')* + '3'
let binStr1 = ['1'; '0'; '0'; '0'] // not acceptable for sndAtm
let binStr2 = ['1'; '0'; '1'; '3'] // acceptable for both


[<Tests>]
let testTree =
    testList "ATMs functions" [

        testCase "accept 1000 by (0, 1)*" <| fun _ ->
            Expect.equal (acceptForTests fstAtm binStr1) true ""


        testCase "accept 1000 by (0, 1)* + 3" <| fun _ ->
            Expect.equal (acceptForTests sndAtm binStr1) false " "


        testCase "accept 1013 by (0, 1)*" <| fun _ ->
            Expect.equal (acceptForTests fstAtm binStr2) false ""


        testCase "accept 1013 by (0, 1)* + 3" <| fun _ ->
            Expect.equal (acceptForTests sndAtm binStr2) true ""

]
