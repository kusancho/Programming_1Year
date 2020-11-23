module sixthTests

open Expecto
open myMatrix

[<Tests>]
let multiplyingSparseMatrixes =
    testList "tests of my function of multiplying" [
        testCase "test #1" <| fun _ ->
            let fstMatrix = SparseMatrix(2, 2, [Coordinates(0<line>, 1<row>)])
            let sndMatrix = SparseMatrix(2, 3, [Coordinates(1<line>, 0<row>);
                                         Coordinates(1<line>, 2<row>)])
            let res = multiplyingSparseMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(0<line>, 0<row>); Coordinates(0<line>, 2<row>)] ""

        testCase "test #2" <| fun _ ->
            let fstMatrix = SparseMatrix(3, 3, [Coordinates(1<line>, 1<row>); Coordinates(2<line>, 2<row>)])
            let sndMatrix = SparseMatrix(3, 1, [Coordinates(2<line>, 0<row>)])
            let res = multiplyingSparseMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(2<line>, 0<row>)] ""

        testCase "test on empty matrix" <| fun _ ->
            let fstMatrix = SparseMatrix(3, 3, [Coordinates(1<line>, 1<row>); Coordinates(2<line>, 2<row>)])
            let sndMatrix = SparseMatrix(3, 9, [])
            let res = multiplyingSparseMatrix fstMatrix sndMatrix
            Expect.equal res.content [] ""

        testCase "test on identity matrix" <| fun _ ->
            let fstMatrix = SparseMatrix(3, 3, [Coordinates(1<line>, 1<row>); Coordinates(2<line>, 2<row>)])
            let sndMatrix = SparseMatrix(3, 3, [Coordinates(0<line>, 0<row>); Coordinates(1<line>, 1<row>)
                                                Coordinates(2<line>, 2<row>)])
            let res = multiplyingSparseMatrix fstMatrix sndMatrix
            Expect.equal res.content [Coordinates(1<line>, 1<row>); Coordinates(2<line>, 2<row>)] ""

        testCase "throw test" <| fun _ ->
            let fstMatrix = SparseMatrix (1,1, [Coordinates(0<line>, 0<row>)])
            let sndMatrix = SparseMatrix (2,1, [Coordinates(1<line>, 0<row>)])
            Expect.throws ( fun _ -> (multiplyingSparseMatrix fstMatrix sndMatrix) |> ignore) "exception doesn't works"
    ]
