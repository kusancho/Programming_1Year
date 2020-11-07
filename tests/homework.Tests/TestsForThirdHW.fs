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

        testEqual fibRec 0 0 "test for rec #1"

        testEqual fibRec 2 1 "test for rec #2"

        testEqual fibIter 1 1 "test for iter #1"

        testEqual fibIter 3 2 "test for iter #2"

        testEqual fibTail 4 3 "test for tail #1"

        testEqual fibTail 5 5 "test for tail #2"

        testEqual fibLogNaive 0 0 "test for naive #1"

        testEqual fibLogNaive 6 8 "test for naive #2"

        testEqual fibLogOptim 8 21 "test for optimized #1"

        testEqual fibLogOptim 7 13 "test for optimized #2"

        testThrow fibRec -1 "recursive"

        testThrow fibIter -13 "iter"

        testThrow fibTail -7 "tail"

        testThrow fibLogNaive -4 "naive"

        testThrow fibLogOptim -2 "optimized"

        testThrow fibSeq -10 "sequences"
    ]

[<Tests>]
let fibAutoTests =
    testList "testProperty for Fib exercises" [

        property fibLogNaive fibLogOptim "naive" "optimized"

        property fibRec fibLogOptim "recursive" "optimized"

        property fibIter fibLogOptim "iter" "optimized"

        property fibRec fibIter "recursive" "iter"

        property fibRec fibTail "recursive" "tail"

        property fibTail fibLogNaive "tail" "naive"
    ]

let throwTests =
    testList "throw tests" [

        testThrow matrixes.identityMatrix -1 "identity matrix #1"

        testThrow matrixes.identityMatrix 0 "identity matrix #2"

        testThrow (matrixes.powMatrixNaively matrixes.matrixForFib) -1 "pow matrix naively"

        testThrow (matrixes.optimizedPow matrixes.matrixForFib) -2 "pow matrix optimized"

        testThrow fibRec -1 "recursive"

        testThrow fibIter -13 "iter"

        testThrow fibTail -7 "tail"

        testThrow fibLogNaive -4 "naive"

        testThrow fibLogOptim -2 "optimized"

        testThrow fibSeq -10 "sequences"
        ]
