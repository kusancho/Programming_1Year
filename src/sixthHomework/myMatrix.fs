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
    new(i,j,k) = { nLines = i; nRows = j; content = k }

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
    let charArr = [|for _ in 1..myMatrix.nLines -> Array.replicate myMatrix.nRows '0'|]
    for coord in myMatrix.content do
        let i = int coord.i
        let j = int coord.j
        charArr.[i].[j] <- '1'
    let arrOfStr = [|for _ in 1..myMatrix.nLines -> "" |]
    for i in 0..myMatrix.nLines - 1 do
        let mutable str = ""
        for j in 0..myMatrix.nRows - 1  do
            str <- str + (string charArr.[i].[j])
        arrOfStr.[i] <- str
    File.WriteAllLines (path, arrOfStr)

let multiplyingBoolMatrix (m1: BoolMatrix) (m2: BoolMatrix) =
    if m1.nRows <> m2.nLines then failwith "can't multiply, wrong size"
    let resList = [
        for firstMult in m1.content do
            for sndMult in m2.content do
                if int firstMult.j = int sndMult.i
                then Coordinates(firstMult.i, sndMult.j) ]
    BoolMatrix(m1.nRows, m2.nLines, List.distinct resList)
