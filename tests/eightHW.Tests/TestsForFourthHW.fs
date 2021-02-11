module TestsForEighthHW

open Expecto
open QuadTree

[<Tests>]
let testSpecArr =
    testList "arrToSpecArr test" [
        testCase "average value" <| fun _ ->
            let arr = [|[|0; 1|]|]
            Expect.equal (arrToSpecArr arr) [|[|0;1|]; [|0; 0|]|] "error on [|[|0; 1|]|]"

        testCase "arrToSpecArr test" <| fun _ ->
            let arr = [|[|1;1|]; [|3; 7|]|]
            Expect.equal (arrToSpecArr arr) [|[|1;1|];[|3;7|]|] "error on [|[|1;1|]; [|3; 7|]|]" ]

[<Tests>]
let testStrToInt =
    testList "testStrToInt test" [
        testCase "testStrToInt #1" <| fun _ ->
            let str = "123"
            Expect.equal (strToInt str) 123 "123"

        testCase "testStrToInt #2" <| fun _ ->
            let str = "00"
            Expect.equal (strToInt str) 0 "00" ]

[<Tests>]
let testsOfFunctionOfArray =
    testList "ofArray test" [
        testCase "ofArray #1" <| fun _ ->
            let str = [|[|2; 4|]|]
            let tree = Node(Data 2, Data 4, Data 0, Data 0)
            Expect.equal (quadTree.ofArray (arrToSpecArr str)) tree "[|[|2; 4|]|]"

        testCase "ofArray #2" <| fun _ ->
            let str = [|[|2; 3|]; [|6; 9|]|]
            let tree = Node(Data 2, Data 3, Data 6, Data 9)
            Expect.equal (quadTree.ofArray (arrToSpecArr str)) tree "[|[|2; 3; 6; 9|]|]"

        testCase "ofArray #3" <| fun _ ->
            let str = [|[|2|]|]
            let tree = Node(Data 2, Data 0, Data 0, Data 0)
            Expect.equal (quadTree.ofArray (arrToSpecArr str)) tree "[|[|2|]|]"
    ]
