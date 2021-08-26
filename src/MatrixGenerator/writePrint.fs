module writePrint


open System.IO


let printMatrix (mtx: 't [,]) path =
    let strMtx = Array2D.map (fun elem -> elem.ToString()) mtx
    let lst = [for i in 0 .. mtx.GetLength(1) - 1 do
                   String.concat " " strMtx.[i, *] ]
    File.WriteAllLines(path, lst)


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
