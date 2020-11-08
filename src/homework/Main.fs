namespace homework
// ПРЕДПОЛАГАЕТСЯ, ЧТО ПОЛЬЗОВАТЕЛЬ НЕ БУДЕТ ВВОДИТЬ СЛИШКОМ БОЛЬШИХ ЧИСЕЛ
module Main =
    open Argu
    open System
    type CLIArguments =
        | FirstExercise1
        | SecondExercise1
        | ThirdExercise1
        | FourthExercise1
        | FifthExercise1
        | SixthExercise1
        | FibonacciRec
        | FibonacciIter
        | FibonacciTail
        | FibonacciLogNaive
        | FibonacciLogOptim
        | FibonacciSeq

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FirstExercise1 -> "start first exercise from second homework"
                | SecondExercise1 -> "start second exercise from second homework"
                | ThirdExercise1 -> "start third exercise from second homework"
                | FourthExercise1 -> "start fourth exercise from second homework"
                | FifthExercise1 -> "start fifth exercise from second homework"
                | SixthExercise1 -> "start sixth exercise from second homework"
                | FibonacciRec -> "start first exercise from third homework"
                | FibonacciIter -> "start second exercise from third homework"
                | FibonacciTail -> "start third exercise from third homework"
                | FibonacciLogNaive -> "start fourth exercise from third homework"
                | FibonacciLogOptim -> "start fifth exercise from third  homework"
                | FibonacciSeq -> "start sixth exercise from third homework"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        let noFlood exercise =
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = exercise  number
            printfn "the result of doing  ex. = %A" current

        if results.Contains FirstExercise1
        then noFlood hw2.firstEx

        if results.Contains SecondExercise1
        then noFlood hw2.secondEx

        if results.Contains ThirdExercise1
        then
            printf "enter the size of array: "
            let size = Console.ReadLine() |> int
            printf "enter max elem: "
            let max = Console.ReadLine() |> int
            let array = hw2.makeArray  size
            let out = hw2.thirdEx  array max
            printf " the result of doing ex: "
            printf "%A" out

        if results.Contains FourthExercise1
        then
            printf "enter the size of array: "
            let mutable size = Console.ReadLine() |> int
            let array = hw2.makeArray size
            printf "enter range (from): "
            let mutable a = Console.ReadLine() |> int
            printf "enter range (to): "
            let mutable b = Console.ReadLine() |> int
            let out = hw2.fourthEx  array a b
            printf " the result of doing ex: "
            printf "%A" out

        if results.Contains FifthExercise1
        then
            printf "enter two elements: "
            let first = Console.ReadLine() |> int
            let second = Console.ReadLine() |> int
            let array = hw2.fifthEx first second
            printf " the result of doing ex: "
            printf "%A" array

        if results.Contains SixthExercise1
        then
            printf "enter size of array: "
            let size = Console.ReadLine() |> int
            let array = hw2.makeArray size
            printf "enter i and j indexes: "
            let mutable i = Console.ReadLine() |> int
            let mutable j = Console.ReadLine() |> int
            let out = hw2.sixthEx array i j
            printf " the result of doing ex: "
            printf "%A" out

        if results.Contains FibonacciRec
        then noFlood hw3.fibRec

        if results.Contains FibonacciIter
        then noFlood hw3.fibIter

        if results.Contains FibonacciTail
        then noFlood hw3.fibTail

        if results.Contains FibonacciLogNaive
        then noFlood hw3.fibLogNaive

        if results.Contains FibonacciLogOptim
        then noFlood hw3.fibLogOptim

        if results.Contains FibonacciSeq
        then noFlood hw3.fibSeq

        else
            parser.PrintUsage() |> printfn "%s"
        0
