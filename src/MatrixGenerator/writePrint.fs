module writePrint


open System.IO


let printMatrix (mtx: seq<seq<string>>) path =
    File.WriteAllLines(path, Seq.map (String.concat " ") mtx)


let readIntMatrix path =
    let strings = File.ReadAllLines path
    let colSize = strings.[0].Split(' ').Length
    [|for i in 0 .. strings.Length - 1 ->
      [|
      let subString = strings.[i].Split(' ')
      if subString.Length <> colSize then failwith "wrong matrix"
      for j in 0 .. colSize - 1 ->
          int <| subString.[j]
      |]
    |]


let evaluateSparsity (mtx: int[][]) =
    let mutable nons = 0.
    let numOfElems = float <| mtx.Length * mtx.[0].Length
    for i in 0 .. mtx.Length - 1 do
        for j in 0 .. mtx.[0].Length - 1 do
            if mtx.[i].[j] = 0
            then nons <- nons + 1.
    nons / numOfElems
