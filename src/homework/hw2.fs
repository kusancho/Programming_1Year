namespace homework
open System
module hw2 =
    let firstEx x =  x * x * x * x + x * x * x + x * x + x + (1) |> int
    let secondEx x =
       let sqrX = (x * x) |> int
       let res = (sqrX * (sqrX + x + 1) + x + 1) |> int
       res
    let makeArray size = // make random array
        if size < 1
        then failwith("wrong size of array (makeArray)")
        let random_array amount =
            let rand = System.Random()
            Array.init amount (fun _ -> rand.Next ())
        let myArray = random_array size
        myArray
    let thirdEx (array: int array) max =
        let mutable j = 0
        let tempArray = Array.zeroCreate array.Length
        for i = 0 to array.Length - 1 do
            if array.[i] <= max
            then
                tempArray.[j] <- i
                j <- j + 1
        if j = 0
        then failwith("indexes doesn't exist")
        else
            let outArray = Array.zeroCreate j
            for l = 0 to j - 1 do
                outArray.[l] <- tempArray.[l]
            outArray
    let fourthEx (array: int array) a b =
        let mutable right = 0
        let mutable left = 0
        if a = b
        then failwith("point isn't open range")
        elif a > b
        then
                right <- a
                left <- b
        else
                right <- b
                left <- a
        let mutable j = 0
        if array.Length < 1
        then failwith("wrong size of array")
        let tempArray = Array.zeroCreate array.Length
        for i = 0 to array.Length - 1 do
            if array.[i] < left || array.[i] > right
            then
                tempArray.[j] <- i
                j <- j + 1
        if j = 0
        then failwith("indexes doesn't exist")
        let outArray = Array.zeroCreate j
        for l = 0 to j - 1 do
            outArray.[l] <- tempArray.[l]
        outArray
    let fifthEx first second =
        let array = [|first;second|]
        array.[0] <- array.[1] + array.[0]
        array.[1] <- array.[0] - array.[1]
        array.[0] <- array.[0] - array.[1]
        array
    let sixthEx (array: int array) i j =
            if (i < 0 || j < 0 || j > array.Length - 1 || i > array.Length - 1)
            then failwith("wrong indexes")
            else
                array.[i] <- array.[j] + array.[i]
                array.[j] <- array.[i] - array.[j]
                array.[i] <- array.[i] - array.[j]
                array
