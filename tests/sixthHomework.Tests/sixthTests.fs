module sixthTests

open Expecto
open myMatrix

[<Tests>]
let multiplyingSparseMatrixes =
    testList "tests of my function of multiplying" [
        testCase "test #1" <| fun _ ->
            let fstMatrix = BoolMatrix(2, 2, [Coordinates(0<line>, 1<col>)])
            let sndMatrix = BoolMatrix(2, 3, [Coordinates(1<line>, 0<col>);
                                         Coordinates(1<line>, 2<col>)])
            let res = multiplyingBoolMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(0<line>, 0<col>); Coordinates(0<line>, 2<col>)] ""

        testCase "test #2" <| fun _ ->
            let fstMatrix = BoolMatrix(3, 3, [Coordinates(1<line>, 1<col>); Coordinates(2<line>, 2<col>)])
            let sndMatrix = BoolMatrix(3, 1, [Coordinates(2<line>, 0<col>)])
            let res = multiplyingBoolMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(2<line>, 0<col>)] ""

        testCase "test on empty matrix" <| fun _ ->
            let fstMatrix = BoolMatrix(3, 3, [Coordinates(1<line>, 1<col>); Coordinates(2<line>, 2<col>)])
            let sndMatrix = BoolMatrix(3, 9, [])
            let res = multiplyingBoolMatrix fstMatrix sndMatrix
            Expect.equal res.content [] ""

        testCase "test on identity matrix" <| fun _ ->
            let fstMatrix = BoolMatrix(3, 3, [Coordinates(1<line>, 1<col>); Coordinates(2<line>, 2<col>)])
            let sndMatrix = BoolMatrix(3, 3, [Coordinates(0<line>, 0<col>); Coordinates(1<line>, 1<col>);
                                                Coordinates(2<line>, 2<col>)])
            let res = multiplyingBoolMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(1<line>, 1<col>); Coordinates(2<line>, 2<col>)] ""

        testCase "throw test" <| fun _ ->
            let fstMatrix = BoolMatrix (1,1, [Coordinates(0<line>, 0<col>)])
            let sndMatrix = BoolMatrix (2,1, [Coordinates(1<line>, 0<col>)])
            Expect.throws ( fun _ -> (multiplyingBoolMatrix fstMatrix sndMatrix) |> ignore) "exception doesn't works"
    ]

let listOfCoordinatesToArrayOfInt (listOfCoordinates: list<Coordinates>) =
    let arrCoordinates = Array.ofList listOfCoordinates
    let arr = Array.zeroCreate (2 * arrCoordinates.Length)
    for i in 0..2..arrCoordinates.Length - 1 do
        arr.[i] <- int arrCoordinates.[i].i
        arr.[i + 1] <- int arrCoordinates.[i].j
    arr

[<Tests>]
let outInput =
    testList "test of output and input function of sparse matrix" [
        testCase "output/input" <| fun _ ->
            let inPath: string = __SOURCE_DIRECTORY__ + "/inputMatrix.txt"
            let outPath: string  = __SOURCE_DIRECTORY__ + "/output.txt"
            let inBoolMatrixFirst = readBoolMatrix inPath
            outBoolMatrix inBoolMatrixFirst outPath
            let inBoolMatrixSecond = readBoolMatrix outPath
            let fstArr = listOfCoordinatesToArrayOfInt inBoolMatrixFirst.content
            let sndArr = listOfCoordinatesToArrayOfInt inBoolMatrixSecond.content
            Expect.equal fstArr sndArr "input output function written wrong"
    ]
