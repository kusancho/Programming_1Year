module writePrint


open System.IO


let printMatrix (mtx: seq<seq<string>>) path =
    if File.Exists path
    then File.Delete path
    File.WriteAllLines(path, Seq.map (fun seq -> String.concat " " seq) mtx)


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
