module MatrixGeneratorTests

open Expecto
open Generator
open System.IO

let path = __SOURCE_DIRECTORY__
let sparsity, lineSize, colSize, amount = 0.2, 5, 5, 1
let config = GeneratorConfig(Int, lineSize, colSize, sparsity, amount, path)
generateMatrix config
let readIntMatrix path =
    let binStrings = File.ReadAllLines path
    let colSize = (binStrings.[0].Split [|' '; '\n'|] ).Length
    [|for i in 0 .. binStrings.Length - 1 ->
      [|
      let subString = binStrings.[i].Split([|' '; '\n'|])
      if subString.Length <> colSize then failwith "wrong matrix"
      for j in 0 .. colSize - 1 ->
          int <| subString.[j]
      |]
    |]



[<Tests>]
let testSparseMatrix =
    testList "MatrixGenerator" [
            testCase "sparsity" <| fun _ ->
                let mtx = readIntMatrix path
                let mutable nons = 0.
                let numOfElems = float <| mtx.Length * mtx.[0].Length
                for i in 0 .. mtx.Length - 1 do
                    for j in 0 .. mtx.[0].Length - 1 do
                        if mtx.[i].[j] = 0
                        then nons <- nons + 1.
                Expect.equal (nons / numOfElems) sparsity ""


]

