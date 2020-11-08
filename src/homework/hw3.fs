namespace homework

module hw3 =
    let rec fibRec n = // rec
        if n < 0
        then failwith ("wrong index of fibnumber")
        elif n = 0 || n = 1
        then n
        else fibRec (n - 1) + fibRec (n - 2)

    let fibIter n = //iter
        if n < 0
        then failwith ("wrong index of fibnumber")
        elif n = 0 || n = 1
        then n
        else
            let mutable a = 0
            let mutable b = 1
            let mutable z = 0
            for i = 2 to n do
                z <- a + b
                a <- b
                b <- z
            z

    let fibTail n = //tail
        if n < 0
        then failwith("wrong number")
        elif n = 1 || n = 0
        then n
        else
            let rec helper n prev curr =
                if n = 0
                then prev
                else helper (n - 1) curr (prev + curr)
            helper n 0 1

    let fibLogNaive n = //naive
        if n < 0
        then failwith("wrong number")
        elif n = 1 || n = 0
        then n
        else
            let m1 = matrixes.matrixForFib
            let out = matrixes.powMatrixNaively m1 (n - 1)
            let result = out.[0].[0]
            result

    let fibLogOptim n = //optimized
        if n < 0
        then failwith("wrong number")
        elif n = 0 || n = 1
        then n
        else
             let out = matrixes.optimizedPow matrixes.matrixForFib (n - 1)
             out.[0].[0]

    let fibSeq n =
       if n < 0
       then failwith("wrong number")
       else
           let array = Array.zeroCreate (n + 1)
           array.[1] <- 1
           for i = 2 to n do
               array.[i] <- array.[i - 1] + array.[i - 2]
           array
