module Tests

open homework
open Expecto

[<Tests>]
let tests =
    testList "samples"
        [
          testCase "test for the sake of test #1" <| fun _ ->
            let test = homework.hw2.firstEx 0
            Expect.equal test 1 "there must be 1"

          testCase "test for the sake of test num.2, #1" <| fun _ ->
            let test = homework.hw2.firstEx  1
            Expect.equal test 5 "there must be 5"

          testCase "test for the sake of test #2" <| fun _ ->
            let test = homework.hw2.secondEx  0
            Expect.equal test 1 "there must be 1"

          testCase "test for the sake of test num.2, #2" <| fun _ ->
            let test = homework.hw2.secondEx  1
            Expect.equal test 5 "there must be 5"

          testCase "ordinary test, #3" <| fun _ ->
            let test = homework.hw2.thirdEx  [|2;6;4|] 3 5
            Expect.equal test [|0;2|] "there must be [|0,2|]"

          testCase "ordinary test, #4" <| fun _ ->
            let test = homework.hw2.fourthEx  [|1;4;5;7;8;5;80;59;36|] 20 100
            Expect.equal test [|0;1;2;3;4;5|] "there must be [|0;1;2;3;4;5|]"

          testCase "ordinary test num 2, #4" <| fun _ ->
            let test = homework.hw2.fourthEx  [|4;6;4;6|] 1 100
            Expect.equal test [|6;6;6|] "there must be [|666|] error array"

          testCase "ordinary test, #5" <| fun _ ->
            let test = homework.hw2.fifthEx 2 1
            Expect.equal test [|1;2|] "there must be [|1;2|]"

          testCase "ordinary test, #6" <| fun _ ->
            let test = homework.hw2.sixthEx  [|3;5;2|] 0 2
            Expect.equal test [|2;5;3|] "there must be [|2;5;3|]"

          testCase "exceptions test #6" <| fun _ ->
            let test = homework.hw2.sixthEx  [|3;5;2|] -1 2
            Expect.equal test [|0;0;0;0|] "there must be [|0;0;0;0|]"
        ]
