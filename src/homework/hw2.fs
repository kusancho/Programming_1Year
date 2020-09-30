namespace homework
open System
module hw2 =
    let firstEx x =  x * x * x * x + x * x * x + x * x + x + (1) |> int
    let secondEx x =
       let sqrX = (x * x) |> int
       let res = (sqrX * (sqrX + x + 1) + x + 1) |> int
       res
    let makeArray size = // make random array
        let random_array amount =
            let rand = System.Random()
            Array.init amount (fun _ -> rand.Next ())
        let myArray = random_array size
        myArray
    let thirdEx ( array: int array ) size max = // function specially for test, because of returning obj
        printf "indexes of elements which are less than entered number:"
        let mutable j = 0
        let tempArray = Array.zeroCreate size
        for i = 0 to size - 1 do
            if array.[i] <= max
            then
                tempArray.[j] <- i
                j <- j + 1
        let outArray = Array.zeroCreate j
        for l = 0 to j - 1 do
            outArray.[l] <- tempArray.[l]
        outArray
    let fourthEx ( array: int array ) a b =
        let mutable flag = true
        let mutable right = 0
        let mutable left = 0
        if a = b
        then flag <- false
        elif a > b
        then
                right <- a
                left <- b
        else
                right <- b
                left <- a
        let mutable j = 0
        if array.Length < 1
        then flag <- false
        let tempArray = Array.zeroCreate array.Length
        for i = 0 to array.Length - 1 do
            if array.[i] < left || array.[i] > right
            then
                tempArray.[j] <- i
                j <- j + 1
        if j = 0
        then flag <- false
        let outArray = Array.zeroCreate j
        for l = 0 to j - 1 do
            outArray.[l] <- tempArray.[l]
        if flag
        then outArray
        else
            let errorOut = [|6;6;6|]
            errorOut
    let fifthEx first second =
        let array = [|first;second|]
        array.[0] <- array.[1] + array.[0]
        array.[1] <- array.[0] - array.[1]
        array.[0] <- array.[0] - array.[1]
        array
    let sixthEx (array: int array) i j =
            if (i < 0 || j < 0 || j > array.Length - 1 || i > array.Length - 1)
            then
                let errorArray = Array.zeroCreate (array.Length + 1)
                errorArray
            else
                array.[i] <- array.[j] + array.[i]
                array.[j] <- array.[i] - array.[j]
                array.[i] <- array.[i] - array.[j]
                array
