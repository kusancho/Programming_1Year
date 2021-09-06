module MatrixGeneratorTests

open Expecto
open Generator
open writePrint


let path = __SOURCE_DIRECTORY__ + "/Matrix0.txt"

let sparsity, colSize, lineSize = 0.2, 100, 100

let config = GeneratorConfig(Int, 100, 100, 0.2, 1, __SOURCE_DIRECTORY__)

let newMatrix =
    generateMatrix config
    readIntMatrix path


[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [
            testProperty "sparsity" <| fun _ ->
                let mtx = newMatrix
                let mutable nons = 0.
                let numOfElems = float <| mtx.Length * mtx.[0].Length
                for i in 0 .. mtx.Length - 1 do
                    for j in 0 .. mtx.[0].Length - 1 do
                        if mtx.[i].[j] = 0
                        then nons <- nons + 1.
                Expect.equal (System.Math.Round(nons / numOfElems, 1)) sparsity ""

            testCase "colSize" <| fun _ ->
                let mtx = newMatrix
                let realColSize = mtx.[0].Length
                Expect.equal realColSize colSize ""

            testCase "lineSize" <| fun _ ->
                let mtx = newMatrix
                let realLineSize = mtx.Length
                Expect.equal realLineSize lineSize ""
]

