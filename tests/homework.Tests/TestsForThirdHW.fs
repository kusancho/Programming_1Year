module TestsForThirdHW

open Expecto
open Microsoft.VisualStudio.TestPlatform.ObjectModel
[<Tests>]
let tests =
    testList "Tests for third homework" [

        testCase "Build identity matrix with  wrong args №1" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.identityMatrix -1 |> ignore) "Exception works"

        testCase "Build identity matrix with  wrong args №2" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.identityMatrix 0 |> ignore) "Exception works"

        testCase "Build identity matrix with  wrong args" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.multiplyMartix
                                        (homework.matrixes.identityMatrix 6)
                                        (homework.matrixes.identityMatrix 9) |> ignore) "Exception works"

        testCase "test pow matrix naively" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.powMatrixnaively
                                        (homework.matrixes.matrixForFib) -1 |> ignore) "Exception works"

        testCase "test optimized pow matrix" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.optimizedPow
                                      homework.matrixes.matrixForFib -1 |> ignore) "Exception works"

        testCase "test optimized pow for 0 pow" <| fun _ ->
            Expect.sequenceEqual (homework.matrixes.optimizedPow homework.matrixes.matrixForFib 0)
                                 (homework.matrixes.identityMatrix 2) "expected [|[|1;1|],[|1;0|]|]"

        testCase "default test for rec №1" <| fun _ ->
            let test = homework.hw3.fibRec 0
            Expect.equal test 0 "there must be 0"

        testCase "default test for rec №2" <| fun _ ->
            let test = homework.hw3.fibRec 2
            Expect.equal test 1 "there must be 1"

        testCase "throw test for rec" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fibRec -1 |> ignore) "exception works"

        testCase "default test for iter №1" <| fun _ ->
            let test = homework.hw3.fibIter 1
            Expect.equal test 1 "there must be 1"

        testCase "default test for iter №2" <| fun _ ->
            let test = homework.hw3.fibIter 3
            Expect.equal test 2 "there must be 2"

        testCase "throw test for iter" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fibIter -1 |> ignore) "exception works"

        testCase "default test for tail №1" <| fun _ ->
            let test = homework.hw3.fibTail 4
            Expect.equal test 3 "there must be 3"

        testCase "default test for tail №2" <| fun _ ->
            let test = homework.hw3.fibTail 5
            Expect.equal test 5 "there must be 5"

        testCase "throw test for tail" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fibTail -1 |> ignore) "exception works"

        testCase "default test for naive №1" <| fun _ ->
            let test = homework.hw3.fourthExercise 0
            Expect.equal test 0 "there must be 0"

        testCase "default test for naive №2" <| fun _ ->
            let test = homework.hw3.fourthExercise 6
            Expect.equal test 8 "there must be 8"

        testCase "throw test for naive" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fourthExercise -1 |> ignore) "exception works"

        testCase "default test for optimized №1" <| fun _ ->
            let test = homework.hw3.fifthExercise 8
            Expect.equal test 21 "there must be 21"

        testCase "default test for optimized №2" <| fun _ ->
            let test = homework.hw3.fifthExercise 7
            Expect.equal test 13 "there must be 13"

        testCase "throw test for optimized" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fifthExercise -6 |> ignore) "exception works"

        testCase "default test for sixth №1" <| fun _ ->
            let test = homework.hw3.sixthExercise 3
            Expect.sequenceEqual test [|0;1;1;2|] "there must be [|0;1;1;2|]"

        testCase "default test for sixth №2" <| fun _ ->
            let test = homework.hw3.sixthExercise 5
            Expect.sequenceEqual test [|0;1;1;2;3;5|] "there must be [|0;1;1;2;3;5|]"

        testCase "throw test for sixth exercise" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.sixthExercise -3 |> ignore) "exception works"

        testProperty "is equal  naive to optimized?" <| fun (n:int) ->
            if abs n > 30
            then
                let temp = n % 10
                Expect.equal (homework.hw3.fifthExercise (abs temp)) (homework.hw3.fourthExercise (abs temp)) "fib naive not equal to optimized"
            else
                Expect.equal (homework.hw3.fifthExercise (abs n)) (homework.hw3.fourthExercise (abs n)) "fib naive not equal to optimized"
    ]






