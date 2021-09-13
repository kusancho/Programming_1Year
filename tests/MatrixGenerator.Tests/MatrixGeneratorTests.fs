module MatrixGeneratorTests

open System
open Expecto
open Generator
open writePrint


let path = __SOURCE_DIRECTORY__ + "/Matrix0.txt"


let newConfig lineSize colSize sparsity = GeneratorConfig(Int, lineSize, colSize, sparsity, 1, __SOURCE_DIRECTORY__)


let newMatrix config =
    generateMatrix config
    readIntMatrix path

let evaluateSparsity (mtx: int[][]) =
    let mutable nons = 0.
    let numOfElems = float <| mtx.Length * mtx.[0].Length
    for i in 0 .. mtx.Length - 1 do
        for j in 0 .. mtx.[0].Length - 1 do
            if mtx.[i].[j] = 0
            then nons <- nons + 1.
    nons / numOfElems


[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [
            testProperty "line size, col size, sparsity" <| fun (a: int, b: int, c: int) ->
                let lineSize, colSize, sparsity = abs a + 1000, abs b + 1000, ((abs c + 13) |> float) * 0.1 % 1.
                let mtx = newMatrix <| newConfig lineSize colSize sparsity
                let realColSize = mtx.[0].Length
                let realLineSize = mtx.Length

                Expect.equal (Math.Round(evaluateSparsity mtx, 2)) (Math.Round(sparsity, 2)) "sparsity does not converge"

                Expect.equal realColSize colSize "line size does not converge"

                Expect.equal realLineSize lineSize "col size does not converge"
]

