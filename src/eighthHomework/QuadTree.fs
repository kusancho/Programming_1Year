module QuadTree

open System
open System.IO

let getPowOfTwo number =
    let mutable aproxNum = 2
    while number > aproxNum do
        aproxNum <- aproxNum * 2
    aproxNum

let strToInt (str: string) =
    let arr = Array.rev <| Array.map (Char.GetNumericValue) (str.ToCharArray())
    let mutable acc = 0.0
    for i in 0 .. arr.Length - 1 do
        acc <- acc + arr.[i] * Math.Pow(10.0, float i)
    int acc

let readMatrixFromFileToArrayRepresentation path =
    let strArr = File.ReadAllLines path
    let length = (strArr.[0].Split [|' '; '\n'|]).Length
    [| for lineN in 0 .. strArr.Length - 1 do
           let tempArr = strArr.[lineN].Split [|' '; '\n'; '\t'|]
           if tempArr.Length <> length then failwith "input isn't a matrix"
           Array.map strToInt tempArr
         |]

let arrToSpecArr (arr: array<array<int>>) =
    let nLines = arr.Length
    let nRows = arr.[0].Length
    let sizeSpec = getPowOfTwo <| max nLines nRows
    [|for lineCtr in 0 .. sizeSpec - 1 do
          if lineCtr >= nLines
          then Array.zeroCreate sizeSpec
          else Array.append arr.[lineCtr] (Array.zeroCreate (sizeSpec - nRows))
          |]

type quadTree<'T> =
    | Data of 'T
    | Node of quadTree<'T> * quadTree<'T> * quadTree<'T> * quadTree<'T>

    static member ofArray (arr: array<array<'T>>) =
        if arr.Length <> 2
        then
            let halfInd = arr.Length / 2
            let temp = [|for i in 0 .. halfInd - 1 do
                             [|for j in 0 .. halfInd - 1 do arr.[0].[0]|] |]
            for i in 0 .. halfInd - 1 do
                for j in 0 .. halfInd - 1 do
                    temp.[i].[j] <- arr.[i].[j]
            let nw = quadTree.ofArray [| for i in 0 .. halfInd - 1 do temp.[i]  |]
            for i in 0 .. halfInd - 1 do
                for j in halfInd .. arr.Length - 1 do
                    temp.[i].[j - halfInd] <- arr.[i].[j]
            let ne = quadTree.ofArray [| for i in 0 .. halfInd - 1 do temp.[i]  |]
            for i in halfInd .. arr.Length - 1 do
                for j in 0 .. halfInd - 1 do
                    temp.[i - halfInd].[j] <- arr.[i].[j]
            let sw = quadTree.ofArray [| for i in 0 .. halfInd - 1 do temp.[i]  |]
            for i in halfInd .. arr.Length - 1 do
                for j in halfInd .. arr.Length - 1 do
                    temp.[i - halfInd].[j - halfInd] <- arr.[i].[j]
            let se = quadTree.ofArray [| for i in 0 .. halfInd - 1 do temp.[i]  |]
            Node(nw, ne, sw, se)
        else Node(Data arr.[0].[0], Data arr.[0].[1], Data arr.[1].[0], Data arr.[1].[1])

    static member depth (tree: quadTree<'T>) =
        let rec hidReal acc tree =
            match tree with
            | Node (fst, _, _, _) -> hidReal (acc + 1) fst
            | Data _ -> acc + 1
        hidReal 0 tree

let rec sumOfTwoIntTree (fstTree: quadTree<'T>) (sndTree: quadTree<'T>) =
        if quadTree.depth fstTree <> quadTree.depth sndTree
        then failwith "sizes of trees isn't equal"
        match (fstTree, sndTree) with
        | (Node (fst1, snd1, third1, fourth1), Node (fst2, snd2, third2, fourth2))  ->
            Node (sumOfTwoIntTree fst1 fst2, sumOfTwoIntTree snd1 snd2,
                  sumOfTwoIntTree third1 third2, sumOfTwoIntTree fourth1 fourth2)
        | (Data fst, Data snd) -> Data (fst + snd)
        | _ -> failwith "error (sum of two trees)"
