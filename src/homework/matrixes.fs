namespace homework

module matrixes =

    let matrixForFib =
         let array = Array.init 2 (fun _ -> Array.create 2 1)
         array.[1].[1] <- 0
         array

    let identityMatrix size =
        if size < 1 then failwith("wrong size")
        let out = Array.init size (fun _ -> Array.create size 0)
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                if i = j then out.[i].[j] <- 1
        out

    let multiplyMartix (m1: array<array<_>>) (m2: array<array<_>>) = // ( m, n ) * ( n , k ) = ( m, k )
        let m = m1.Length
        let n = m1.[0].Length
        let linesM2 = m2.Length
        let k = m2.[0].Length
        if not (n = linesM2) then failwith (" ( m, n ) * ( n , k ) = ( m, k ) ")
        let res = Array.init m (fun _ -> Array.zeroCreate k)
        for i = 0 to m - 1 do
            for j = 0 to k - 1 do
                for l = 0 to n - 1 do
                    res.[i].[j] <- res.[i].[j] + m1.[i].[l] * m2.[l].[j]
        res

    let rec powMatrixNaively (m1: array<array<int>>) pow =
        if not (m1.Length = m1.[0].Length) then failwith("matrix should be M*M ")
        if pow < 0
        then failwith("i cant pow in numbers < 0 ")
        elif pow = 0
        then identityMatrix m1.Length
        else multiplyMartix m1 (powMatrixNaively m1 (pow - 1))

    let rec optimizedPow (m1: array<array<int>>) pow =
        if not (m1.Length = m1.[0].Length) then failwith("matrix should be M*M")
        if pow < 0
        then failwith("i cant pow in numbers < 0 ")
        elif pow = 0
        then identityMatrix m1.Length
        elif pow % 2 = 0
        then
            let even = optimizedPow m1 (pow / 2)
            multiplyMartix even even
        else
            let odd = optimizedPow m1 ((pow - 1) / 2)
            multiplyMartix m1 (multiplyMartix odd odd)
