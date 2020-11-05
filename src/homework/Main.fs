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
        | FirstExercise2
        | SecondExercise2
        | ThirdExercise2
        | FourthExercise2
        | FifthExercise2
        | SixthExercise2

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FirstExercise1 -> "start first exercise from second homework"
                | SecondExercise1 -> "start second exercise from second homework"
                | ThirdExercise1 -> "start third exercise from second homework"
                | FourthExercise1 -> "start fourth exercise from second homework"
                | FifthExercise1 -> "start fifth exercise from second homework"
                | SixthExercise1 -> "start sixth exercise from second homework"
                | FirstExercise2 -> "start first exercise from third homework"
                | SecondExercise2 -> "start second exercise from third homework"
                | ThirdExercise2 -> "start third exercise from third homework"
                | FourthExercise2 -> "start fourth exercise from third homework"
                | FifthExercise2 -> "start fifth exercise from third  homework"
                | SixthExercise2 -> "start sixth exercise from third homework"

    [<EntryPoint>]
    let main (argv: string array) =
        printfn "dont enter too big numbers"
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        let copyPaste exercise =
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = exercise number
            printfn "the result of doing fist ex. = %A" current

        if results.Contains FirstExercise1
        then copyPaste hw2.firstEx

        if results.Contains SecondExercise1
        then copyPaste hw2.secondEx

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

        if results.Contains FirstExercise2
        then copyPaste hw3.firstExercise

        if results.Contains SecondExercise2
        then copyPaste hw3.secondExercise

        if results.Contains ThirdExercise2
        then copyPaste hw3.thirdExercise

        if results.Contains FourthExercise2
        then copyPaste hw3.fourthExercise

        if results.Contains FifthExercise2
        then copyPaste hw3.fifthExercise

        if results.Contains SixthExercise2
        then copyPaste hw3.sixthExercise

        else
            parser.PrintUsage() |> printfn "%s"
        0
