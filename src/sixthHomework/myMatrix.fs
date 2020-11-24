module myMatrix

open System.IO

[<Measure>] type line
[<Measure>] type col

[<Struct>]
type Coordinates =
    val i: int<line>
    val j: int<col>
    new(I,J) = {i = I; j = J }

[<Struct>]
type BoolMatrix =
    val nLines: int
    val nRows: int
    val content: list<Coordinates>
    new(i,j,k) = { nLines = i; nRows = j; content = List.sort k }

let readBoolMatrix path =
    let binString = File.ReadAllLines path
    let length = binString.[0].Length
    let listOfCoordinates =
         [
          for i in 0..binString.Length - 1 do
              if binString.[i].Length <> length then failwith "wrong matrix"
              let chars = binString.[i]
              for j in 0..binString.[i].Length - 1 do
                  if chars.[j] = char "1" then Coordinates(i*1<line>, j*1<col>)
          ]
    BoolMatrix(binString.Length, length, listOfCoordinates)

let outBoolMatrix (myMatrix: BoolMatrix) path =
    let mutable content = myMatrix.content
    let mutable out = ""
    for i in 0..myMatrix.nLines - 1 do
        for j in 0..myMatrix.nRows - 1 do
            if not content.IsEmpty
            then
                let coordinate = content.Head
                if coordinate.i = i*1<line> && coordinate.j = j*1<col>
                then
                    content <- content.Tail
                    out <- out + "1"
                else out <- out + "0"
            else out <- out + "0"
        out <- out + "\n"
    File.WriteAllText (path, out)

let multiplyingBoolMatrix (m1: BoolMatrix) (m2: BoolMatrix) =
    if m1.nRows <> m2.nLines then failwith "can't multiply, wrong size"
    let resList = [
        for firstMult in m1.content do
            for sndMult in m2.content do
                if int firstMult.j = int sndMult.i
                then Coordinates(firstMult.i, sndMult.j) ]
    BoolMatrix(m1.nRows, m2.nLines, List.distinct resList)
