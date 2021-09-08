module MatrixGeneratorTests

open System
open Expecto
open Generator
open writePrint


let path = __SOURCE_DIRECTORY__ + "/Matrix0.txt"


let newConfig size =
    match size with
    | line, col, sparsity -> GeneratorConfig(Int, line, col, sparsity, 1, __SOURCE_DIRECTORY__)


let newMatrix config =
    generateMatrix config
    readIntMatrix path


[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [
            testProperty "line size, col size, sparsity" <| fun (size: int * int * int) ->
                let lineSize, colSize , sparsity =
                    match size with
                    | a, b, c ->
                        abs a + 100,
                        abs b + 100,
                        ((abs c + 3) |> float) * 0.1 % 1.
                let mtx = newMatrix <| newConfig (lineSize, colSize, sparsity)
                let realColSize = mtx.[0].Length
                let realLineSize = mtx.Length
                let mutable nons = 0.
                let numOfElems = float <| mtx.Length * mtx.[0].Length
                for i in 0 .. mtx.Length - 1 do
                    for j in 0 .. mtx.[0].Length - 1 do
                        if mtx.[i].[j] = 0
                        then nons <- nons + 1.

                Expect.equal (Math.Round(nons / numOfElems, 1)) (Math.Round(sparsity, 1)) "sparsity does not converge"

                Expect.equal realColSize colSize "line size does not converge"

                Expect.equal realLineSize lineSize "col size does not converge"
]

