module TestsForThirdHW

open Expecto
open homework.hw3
open Microsoft.VisualStudio.TestPlatform.ObjectModel

let testEqual func_name (testValue: int) (expectedValue: int) (description: string) =
    testCase description <| fun _ ->
        let test = func_name testValue
        Expect.equal test expectedValue (sprintf "there must be: %A" expectedValue)

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
            Expect.throws (fun _ -> homework.matrixes.powMatrixNaively
                                        (homework.matrixes.matrixForFib) -1 |> ignore) "Exception works"

        testCase "test optimized pow matrix" <| fun _ ->
            Expect.throws (fun _ -> homework.matrixes.optimizedPow
                                      homework.matrixes.matrixForFib -1 |> ignore) "Exception works"

        testCase "test optimized pow for 0 pow" <| fun _ ->
            Expect.sequenceEqual (homework.matrixes.optimizedPow homework.matrixes.matrixForFib 0)
                                 (homework.matrixes.identityMatrix 2) "expected [|[|1;1|],[|1;0|]|]"

        testEqual firstExercise 0 0 "test for rec #1"

        testEqual firstExercise 2 1 "test for rec #2"

        testEqual secondExercise 1 1 "test for iter #1"

        testEqual secondExercise 3 2 "test for iter #2"

        testEqual thirdExercise 4 3 "test for tail #1"

        testEqual thirdExercise 5 5 "test for tail #2"

        testEqual thirdExercise 0 0 "test for naive #1"

        testEqual thirdExercise 6 8 "test for naive #2"

        testEqual thirdExercise 8 21 "test for optimized #1"

        testEqual thirdExercise 7 13 "test for optimized #2"

        testCase "throw test for rec" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.firstExercise -1 |> ignore) "exception works"

        testCase "throw test for iter" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.secondExercise -1 |> ignore) "exception works"

        testCase "throw test for tail" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.thirdExercise -1 |> ignore) "exception works"

        testCase "throw test for naive" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fourthExercise -1 |> ignore) "exception works"

        testCase "throw test for optimized" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.fifthExercise -6 |> ignore) "exception works"

        testCase "throw test for sixth exercise" <| fun _ ->
            Expect.throws (fun _ -> homework.hw3.sixthExercise -3 |> ignore) "exception works"

        testCase "default test for sixth №1" <| fun _ ->
            let test = homework.hw3.sixthExercise 3
            Expect.sequenceEqual test [|0;1;1;2|] "there must be [|0;1;1;2|]"

        testCase "default test for sixth №2" <| fun _ ->
            let test = homework.hw3.sixthExercise 5
            Expect.sequenceEqual test [|0;1;1;2;3;5|] "there must be [|0;1;1;2;3;5|]"
    ]

let property firstFunc secondFunc (firstMsg: string) (secondMsg: string) =
    testProperty (sprintf "is %A to %A?" firstMsg secondMsg) <| fun (n:int) ->
        let n' = abs <| if abs n > 30 then n % 10 else n
        Expect.equal (firstFunc n') (secondFunc n') (sprintf "%A not equal to %A" firstMsg secondMsg)

[<Tests>]

let FibAutoTests =
    testList "testProperty for Fib exercises" [

        property fourthExercise fifthExercise "naive" "optimized"

        property firstExercise fifthExercise "recursive" "optimized"

        property secondExercise fifthExercise "iter" "optimized"

        property firstExercise secondExercise "recursive" "iter"

        property firstExercise thirdExercise "recursive" "tail"

        property thirdExercise fourthExercise "tail" "naive"

    ]





