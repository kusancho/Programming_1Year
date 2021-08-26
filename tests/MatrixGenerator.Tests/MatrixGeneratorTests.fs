module MatrixGeneratorTests

open Expecto
open writePrint


// CONFIG - sparsity: 0.2 lineSize: 100 colSize: 100 amount: 1
let path = __SOURCE_DIRECTORY__ + "/Matrix0.txt"

let sparsity, colSize, lineSize = 0.2, 100, 100


let mtx = readIntMatrix path


[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [

            testCase "sparsity" <| fun _ ->
                let mutable nons = 0.
                let numOfElems = float <| mtx.Length * mtx.[0].Length
                for i in 0 .. mtx.Length - 1 do
                    for j in 0 .. mtx.[0].Length - 1 do
                        if mtx.[i].[j] = 0
                        then nons <- nons + 1.
                Expect.equal (System.Math.Round(nons / numOfElems, 1)) sparsity ""

            testCase "colSize" <| fun _ ->
                let realColSize = mtx.[0].Length
                Expect.equal realColSize colSize ""

            testCase "lineSize" <| fun _ ->
                let realLineSize = mtx.Length
                Expect.equal realLineSize lineSize ""
]

