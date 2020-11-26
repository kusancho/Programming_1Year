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
                  if chars.[j] = char "1" then Coordinates(i * 1<line>, j * 1<col>)
          ]
    BoolMatrix(binString.Length, length, listOfCoordinates)

let outBoolMatrix (myMatrix: BoolMatrix) path =
    let rec makeString line col string (content: list<Coordinates>) =
        if line = myMatrix.nLines && col = 0
        then string
        elif line = myMatrix.nRows
        then
            if not content.IsEmpty && content.Head.i = line * 1<line> && content.Head.j = col * 1<col>
            then makeString (line + 1) 0 (string + "1" + "\n") content.Tail
            else makeString (line + 1) 0 (string + "0" + "\n") content
        else
            if not content.IsEmpty && content.Head.i = line * 1<line> && content.Head.j = col * 1<col>
            then makeString line (col + 1) (string + "1") content.Tail
            else makeString line (col + 1) (string + "0") content
    let str = makeString 0 0 "" (List.sort myMatrix.content)
    File.WriteAllText (path, str)

let multiplyingBoolMatrix (m1: BoolMatrix) (m2: BoolMatrix) =
    if m1.nRows <> m2.nLines then failwith "can't multiply, wrong size"
    let resList = [
        for firstMult in m1.content do
            for sndMult in m2.content do
                if int firstMult.j = int sndMult.i
                then Coordinates(firstMult.i, sndMult.j) ]
    BoolMatrix(m1.nRows, m2.nLines, List.distinct resList)
