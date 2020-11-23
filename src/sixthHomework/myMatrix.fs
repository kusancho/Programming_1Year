module myMatrix

open System.IO

[<Measure>] type line
[<Measure>] type row

[<Struct>]
type Coordinates =
    val i: int<line>
    val j: int<row>
    new(I,J) = {i = I; j = J }

[<Struct>]
type SparseMatrix =
    val nLines: int
    val nRows: int
    val content: list<Coordinates>
    new(i,j,k) = { nLines = i; nRows = j; content = k }

let readMyMatrix path =
    let binString = File.ReadAllLines path
    let length = binString.[0].Length
    let listOfCoordinates =
         [
          for i in 0..binString.Length - 1 do
              if binString.[i].Length <> length then failwith "wrong matrix"
              let chars = binString.[i]
              for j in 0..binString.[i].Length - 1 do
                  if chars.[j] = char "1" then yield Coordinates(i*1<line>, j*1<row>)
          ]
    SparseMatrix(binString.Length, length, listOfCoordinates)

let outSparseMatrix (myMatrix: SparseMatrix) path =
    let mutable content = myMatrix.content
    let mutable out = ""
    for i in 0..myMatrix.nLines - 1 do
        for j in 0..myMatrix.nRows - 1 do
            if not content.IsEmpty
            then
                let coordinate = content.Head
                if coordinate.i = i*1<line> && coordinate.j = j*1<row>
                then
                    content <- content.Tail
                    out <- out + "1"
                else out <- out + "0"
            else out <- out + "0"
        out <- out + "\n"
    File.WriteAllText (path, out)

let multiplyingSparseMatrix (m1: SparseMatrix) (m2: SparseMatrix) =
    if m1.nRows <> m2.nLines then failwith "can't multiply, wrong size"
    let mutable resList = []
    for firstMult in m1.content do
        for sndMult in m2.content do
            if int firstMult.j = int sndMult.i
            then resList <- resList @ [Coordinates(firstMult.i, sndMult.j)]
    SparseMatrix(int m1.nRows, int m2.nLines, resList)


















