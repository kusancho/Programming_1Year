module TestsForFourthHW

open Expecto
let templateTestCase func value =
    testCase (sprintf "test of %A" func) <| fun _ ->
        let test = func value
        Expect.equal test value (sprintf "there must be: %A" value)

[<Tests>]
let testSortsOfArray =
    testList "tests of my sort of array" [
        templateTestCase sorts.bubbleSortA [||]

    ]

let testSortsOfLists =
    testList "tests of my sort of list" [


    ]


