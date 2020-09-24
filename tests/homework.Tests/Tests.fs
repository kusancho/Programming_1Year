module Tests


open Expecto
open homework

[<Tests>]
let tests =
    testList "samples"
        [ testCase "test for the sake of test #1" <| fun _ ->
            let test=homework.hw2.first_ex 0
            Expect.equal test 1 "there must be 1"
          testCase "test for the sake of test #2" <| fun _ ->
            let test=homework.hw2.second_ex 0
            Expect.equal test 1 "there must be 1"
          testCase "ordinary test, #3" <| fun _ ->

            let test=homework.hw2.third_ex_test  [|2;6;4|] 3 5
            Expect.equal test [|0;2|] "there must be [|0,2|]"
          testCase "ordinary test, #4" <| fun _ ->
            let test = homework.hw2.fourth_ex_test [|1;4;5;7;8;5;80;59;36|] 20 100
            Expect.equal test [|0;1;2;3;4;5|] "there must be [|0;1;2;3;4;5|]"
            ]

