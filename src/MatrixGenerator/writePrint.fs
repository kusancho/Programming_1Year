module writePrint


open System.IO


let printMatrix (mtx: 't [,]) path =
    if File.Exists path
    then File.Delete path
    let strMtx = Array2D.map (fun elem -> elem.ToString()) mtx
    let size = mtx.GetLength(1) - 1
    for i in 0 .. size - 1 do
        File.AppendAllText(path, String.concat " " strMtx.[i, *] + "\n")
    File.AppendAllText(path, String.concat " " strMtx.[size, *])


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
