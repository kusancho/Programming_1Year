namespace sortProj

open System
module fourthHomework =

    let readFile file =
        try
            let a = System.IO.File.ReadAllLines file
            let intArr = Array.zeroCreate a.Length
            let mutable j = 0
            for i in a do
                intArr.[j] <- (i.Trim())
                j <- j + 1
            intArr
        with
        | :? System.IO.FileNotFoundException ->
            failwith "Given file has not been found \n\n\n"
        | :? System.IO.IOException ->
            failwith "Invalid file name or  wrong data in file\n\n\n"
        | :? ArgumentException ->
            failwith "Empty file path given\n\n\n"
        | :? FormatException ->
            failwith "Wrong format of data \n Hint: one int in line\n\n\n"

    let readIntList file = // :)
        List.ofArray (Array.map int (readFile file))

    let readIntArray file =
        Array.map int (readFile file)

    let outArray path (arr: array<_>) =
        let mutable content = ""
        for i in 0 .. arr.Length - 1 do
            content <- content + (sprintf "%A" arr.[i]) + "\n"
        System.IO.File.WriteAllText (path, content)

    let outList path list =
        outArray path (Array.ofList list)

    let bSortArrayFromFile file = //bubble sort for array
        let array = readIntArray file
        let out = sorts.bubbleSortA array
        out

    let qSortArrayFromFile file = //qsort for array
        let array = readIntArray file
        sorts.quickSortA array

    let bSortListFromFile file = //bubble sort for list
        let list = readIntList file
        let out = sorts.bubbleSortL list
        out

    let qSortListFromFile file = //quick sort for list
        let list = readIntList file
        let out = sorts.quickSortL list
        out

    let pack32To64 (first: int32) (second: int32) =
        if second < 0
        then (int64 (first + 1) <<< 32) + (int64 second)
        else (int64 first <<< 32) + (int64 second)

    let unpack64To32 (bigInt: int64) =
        let first = bigInt >>> 32 |> int32
        let second = (bigInt <<< 32) >>> 32 |> int32
        first, second

    let pack16To64 (first: int16) (second: int16) (third: int16) (fourth: int16) =
        let first32 = if second < 0s then (int32 (first + 1s) <<< 16) + (int32 second) else (int32 first <<< 16) + (int32 second)
        let second32 = if fourth < 0s then (int32 (third + 1s) <<< 16) + (int32 fourth) else (int32 third <<< 16) + (int32 fourth)
        pack32To64 first32 second32

    let unpack64To16 (bigInt: int64) =
        let fourth = bigInt |> int16          // example why it will work:  x(int8) = 11101010 in bin. system
        let third = bigInt >>> 16 |> int16    // x |> int4 1010 in bin. system.
        let second = bigInt >>> 32 |> int16
        let first = bigInt >>> 48 |> int16
        first, second, third, fourth
