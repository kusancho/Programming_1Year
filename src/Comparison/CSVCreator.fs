module Comparison


let time f =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let res = f()
    let time = timer.ElapsedMilliseconds
    time


let createCSV seqPath parallelPath sMult pMult _from _to _step cycles sparsity =
    let sizes = [for j in _from .. _step .. _to -> j]
    let fstTime, sndTime = Array.zeroCreate sizes.Length, Array.zeroCreate sizes.Length
    let mutable j = 0
    for size in sizes do
        let tree = QuadTree.extendedTree<int>.createSquareIntQT size sparsity
        let fstTimes, sndTimes = Array.zeroCreate cycles, Array.zeroCreate cycles
        for j in 0 .. cycles - 1 do
            fstTimes.[j] <- string <| time (fun _ -> sMult tree tree)
            sndTimes.[j] <- string <| time (fun _ -> pMult tree tree)
        fstTime.[j] <- (string size) + ","  + (String.concat ", " fstTimes)
        sndTime.[j] <- (string size) + ","  + (String.concat ", " sndTimes)
        j <- j + 1
    System.IO.File.WriteAllLines(seqPath, fstTime)
    System.IO.File.WriteAllLines(parallelPath, sndTime)
