module TestsForFourthHW

open Expecto
open sortProj

let templateTestCase func value =
    testCase (sprintf "test of %A" func) <| fun _ ->
        let test = func value
        Expect.equal test value (sprintf "there must be: %A" value)

let propertyTesting func1 func2 msg =
    testProperty (sprintf "comparison of %A with system sort " msg) <| fun n ->
        Expect.sequenceEqual (func1 n) (func2 n)

[<Tests>]
let testSortsOfArrayAndList =
    testList "tests of my sort of array (special cases)" [
        templateTestCase sorts.bubbleSortA [||]
        templateTestCase sorts.bubbleSortA [|0|]
        templateTestCase sorts.quickSortA [||]
        templateTestCase sorts.quickSortA [|0|]
        templateTestCase sorts.bubbleSortL []
        templateTestCase sorts.bubbleSortL [0]
        templateTestCase sorts.quickSortL []
        templateTestCase sorts.quickSortL [0]
    ]

[<Tests>]
let propertyTestsOfArray =
    testList "tests of my sorts of array on random data" [
        propertyTesting Array.sort sorts.bubbleSortA "bubble sort array"
        propertyTesting Array.sort sorts.quickSortA "quick sort array"
    ]

[<Tests>]
let propertyTestsOfPackUnpack =
    testList "tests of pack unpack" [
        testProperty "pack 16 to 64" <| fun (a: int16,b: int16,c: int16,d: int16) ->
            Expect.equal (a,b,c,d) (fourthHomework.unpack64To16 (fourthHomework.pack16To64 a b c d)) ""
        testProperty "pack" <| fun (a: int32,b: int32) ->
            Expect.equal (a,b) (fourthHomework.unpack64To32 (fourthHomework.pack32To64 a b)) ""
    ]

let propertyTestsOfList =
    testList "tests of my sorts of list on random data" [
        propertyTesting List.sort sorts.bubbleSortL "bubble sort list"
        propertyTesting List.sort sorts.quickSortL "quick sort list"
    ]


