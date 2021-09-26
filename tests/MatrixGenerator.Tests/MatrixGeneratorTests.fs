module MatrixGeneratorTests

open System
open Expecto
open Generator
open writePrint


let fileName = "/Matrix0.txt"
let pathSparsity = __SOURCE_DIRECTORY__ + "/sparsity"
let pathLineSize = __SOURCE_DIRECTORY__ + "/lineSize"
let pathColSize = __SOURCE_DIRECTORY__ + "/colSize"


let newConfig lineSize colSize sparsity path = GeneratorConfig(Int, lineSize, colSize, sparsity, 1, path)


let newMatrix config path =
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


let argsToNormal a b c =
    abs a + 100, abs b + 100, ((abs c + 13) |> float) * 0.1 % 1.


[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [
            testProperty "sparsity" <| fun (a: int, b: int, c: int) ->
                let lineSize, colSize, sparsity = argsToNormal a b c
                let mtx = newMatrix (newConfig lineSize colSize sparsity pathSparsity) <| pathSparsity + fileName
                Expect.equal (sparsity - 0.05 <= Math.Round(evaluateSparsity mtx, 6)
                              && Math.Round(evaluateSparsity mtx, 6) <= sparsity + 0.05)
                               true
                               "sparsity does not converge"


            testProperty "line size" <| fun (a: int, b: int, c: int) ->
                let lineSize, colSize, sparsity = argsToNormal a b c
                let mtx = newMatrix (newConfig lineSize colSize sparsity pathLineSize) <| pathLineSize + fileName
                let realLineSize = mtx.Length
                Expect.equal realLineSize lineSize "col size does not converge"


            testProperty "col size" <| fun (a: int, b: int, c: int) ->
                let lineSize, colSize, sparsity = argsToNormal a b c
                let mtx = newMatrix (newConfig lineSize colSize sparsity pathColSize) <| pathColSize + fileName
                let realColSize = mtx.[0].Length
                Expect.equal realColSize colSize "line size does not converge"
]

