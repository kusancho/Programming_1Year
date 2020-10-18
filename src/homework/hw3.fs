namespace homework

module hw3 =
    let rec fibRec n =
        if n < 0 then failwith ("wrong index of fibnumber")
        else if n = 0 || n = 1 then n
        else fibRec (n - 1) + fibRec (n - 2)

    let fibIter n =
        if n < 0 then failwith ("wrong index of fibnumber")
        else if n = 0 || n = 1 then n
        else
            let mutable a = 0
            let mutable b = 1
            let mutable z = 0
            for i = 2 to n do
                z <- a + b
                a <- b
                b <- z
            z

    let fibTail n =
        if n < 0 then failwith("wrong number")
        else if n = 1 || n = 0 then n
        else
            let rec _tail n prev curr =
             if n = 0
             then prev
             else _tail (n - 1) curr (prev + curr)
            _tail n 0 1

    let fourthExercise n =
        if n < 0 then failwith("wrong number")
        else if n = 1 || n = 0 then n
        else
            let m1 = homework.matrixes.matrixForFib
            let out = homework.matrixes.powMatrixnaively m1 (n - 1)
            let result = out.[0].[0]
            result

    let fifthExercise n =
        if n < 0 then failwith("wrong number")
        else if n = 0 || n = 1 then n
        else let out = homework.matrixes.optimizedPow homework.matrixes.matrixForFib (n - 1)
             out.[0].[0]

    let sixthExercise n =
       if n < 0 then failwith("wrong number")
       else
           let array = Array.zeroCreate (n+1)
           array.[1] <- 1
           for i = 2 to n do
               array.[i] <- array.[i - 1] + array.[i - 2]
           array
