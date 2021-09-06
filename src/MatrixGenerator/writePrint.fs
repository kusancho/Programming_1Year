module writePrint


open System.IO


let printMatrix (mtx: seq<seq<'t>>) path =
    let size = Seq.length mtx - 1
    let writeToFile str =
        File.AppendAllText(path, str)

    let space cntr =
        if cntr <> size then " " else ""

    if File.Exists path
    then File.Delete path
    for s in mtx do
        Seq.iteri (fun i symb -> writeToFile <| (symb.ToString() + space i)) s
        writeToFile "\n"


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
