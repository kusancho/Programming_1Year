namespace homework

open System

module fourthHomework =

    let readArray file =
        try
            let a = System.IO.File.ReadAllLines file
            let intArr = Array.zeroCreate a.Length
            let mutable j = 0
            for i in a do
                intArr.[j] <- int (i.Trim())
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

    let firstEx file = //bubble sort for array
        let array = readArray file
        let out = sorts.bubbleSortA array
        printfn "sorted array: %A" out

    let secondEx file = //qsort for array
        let array = readArray file
        sorts.quickSortA &array  //ref
        printfn "sorted array: %A" array

    let thirdEx file = //bubble sort for list
        let list = List.ofArray <| readArray file
        let out = sorts.bubbleSortL list            //не совсем копипаст, поэтому отдельными функциями
        printfn "sorted list: %A" out

    let fourthEx file = //quick sort for list
        let list = List.ofArray <| readArray file
        let out = sorts.quickSortL list
        printfn "sorted list: %A" out

    let pack32To64 (first: int32) (second: int32) =
        if second < 0
        then (int64 (first + 1) <<< 32) + (int64 second)
        else (int64 first <<< 32) + (int64 second)

    let unpack64To32 (bigInt: int64) =
        let first = bigInt >>> 32 |> int32
        let second = (bigInt <<< 32) >>> 32 |> int32
        first, second

    let fifthEx (array: array<int32>) = //(pack-unpack) in case when input (second argument) is negative number, first returned number less by one than first input number, function unpack64To32 too primitive to be problematic, so i improve pack function
        let bigInt = pack32To64 array.[0] array.[1]
        let first', second' = unpack64To32 bigInt
        printfn "packed: %A" bigInt
        printfn "unpacked 32 numbers: \n %A \n %A" first' second'

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

    let sixthEx (array: array<int16>) =
        let bigInt = pack16To64 array.[0] array.[1] array.[2] array.[3]
        let first', second', third', fourth' = unpack64To16 bigInt
        printfn "packed: %A" bigInt
        printfn "unpacked int16 numbers: \n %A \n %A \n %A \n %A" first' second' third' fourth'
