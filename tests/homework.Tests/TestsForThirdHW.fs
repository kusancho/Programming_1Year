module TestsForThirdHW

open Expecto
open homework.hw3
open homework

let testEqual func_name (testValue: int) (expectedValue: int) (description: string) =
    testCase description <| fun _ ->
        let test = func_name testValue
        Expect.equal test expectedValue (sprintf "there must be: %A" expectedValue)

let testThrow func_name (testValue: int) (name: string) =
    testCase (sprintf "throw test for %A" name) <| fun _ ->
        Expect.throws (fun _ -> func_name testValue |> ignore) "exception doesn't works"

let property firstFunc secondFunc (firstMsg: string) (secondMsg: string) =
    testProperty (sprintf "is %A to %A?" firstMsg secondMsg) <| fun (n:int) ->
        let n' = abs <| if abs n > 30 then n % 10 else n
        Expect.equal (firstFunc n') (secondFunc n') (sprintf "%A not equal to %A" firstMsg secondMsg)

[<Tests>]
let equalTests =
    testList "equalTests" [

        testEqual firstExercise 0 0 "test for rec #1"

        testEqual firstExercise 2 1 "test for rec #2"

        testEqual secondExercise 1 1 "test for iter #1"

        testEqual secondExercise 3 2 "test for iter #2"

        testEqual thirdExercise 4 3 "test for tail #1"

        testEqual thirdExercise 5 5 "test for tail #2"

        testEqual fourthExercise 0 0 "test for naive #1"

        testEqual fourthExercise 6 8 "test for naive #2"

        testEqual fifthExercise 8 21 "test for optimized #1"

        testEqual fifthExercise 7 13 "test for optimized #2"

        testThrow firstExercise -1 "recursive"

        testThrow secondExercise -13 "iter"

        testThrow thirdExercise -7 "tail"

        testThrow fourthExercise -4 "naive"

        testThrow fifthExercise -2 "optimized"

        testThrow sixthExercise -10 "sequences"
    ]

[<Tests>]
let fibAutoTests =
    testList "testProperty for Fib exercises" [

        property fourthExercise fifthExercise "naive" "optimized"

        property firstExercise fifthExercise "recursive" "optimized"

        property secondExercise fifthExercise "iter" "optimized"

        property firstExercise secondExercise "recursive" "iter"

        property firstExercise thirdExercise "recursive" "tail"

        property thirdExercise fourthExercise "tail" "naive"
    ]

let throwTests =
    testList "throw tests" [

        testThrow matrixes.identityMatrix -1 "identity matrix #1"

        testThrow matrixes.identityMatrix 0 "identity matrix #2"

        testThrow (matrixes.powMatrixNaively matrixes.matrixForFib) -1 "pow matrix naively"

        testThrow (matrixes.optimizedPow matrixes.matrixForFib) -2 "pow matrix optimized"

        testThrow firstExercise -1 "recursive"

        testThrow secondExercise -13 "iter"

        testThrow thirdExercise -7 "tail"

        testThrow fourthExercise -4 "naive"

        testThrow fifthExercise -2 "optimized"

        testThrow sixthExercise -10 "sequences"
        ]
