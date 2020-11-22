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
type MyMatrix =
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
    MyMatrix(binString.Length, length, listOfCoordinates)
