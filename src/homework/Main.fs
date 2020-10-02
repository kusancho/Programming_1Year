namespace homework
// ПРЕДПОЛАГАЕТСЯ, ЧТО ПОЛЬЗОВАТЕЛЬ НЕ БУДЕТ ВВОДИТЬ СЛИШКОМ БОЛЬШИХ ЧИСЕЛ
module Main =
    open Argu
    open System
    type CLIArguments =
        | FirstExercise
        | SecondExercise
        | ThirdExercise
        | FourthExercise
        | FifthExercise
        | SixthExercise

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FirstExercise -> "start first exercise"
                | SecondExercise -> "start second exercise"
                | ThirdExercise -> "start third exercise"
                | FourthExercise -> "start fourth exercise"
                | FifthExercise -> "start fifth exercise"
                | SixthExercise -> "start sixth exercise"

    [<EntryPoint>]
    let main (argv: string array) =
        printfn "dont enter too big numbers"
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        if results.Contains FirstExercise
        then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.firstEx  number
            printfn "the result of doing fist ex. = %A" current

        if results.Contains SecondExercise
        then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.secondEx  number
            printfn "the result of doing second ex. = %A" current

        if results.Contains ThirdExercise
        then
            printf "enter the size of array: "
            let size = Console.ReadLine() |> int
            printf "enter max elem: "
            let max = Console.ReadLine() |> int
            let array = homework.hw2.makeArray  size
            let out = homework.hw2.thirdEx  array max
            printf " the result of doing ex: "
            printf "%A" out

        if results.Contains FourthExercise
        then
            printf "enter the size of array: "
            let mutable size = Console.ReadLine() |> int
            let array = homework.hw2.makeArray size
            printf "enter range (from): "
            let mutable a = Console.ReadLine() |> int
            printf "enter range (to): "
            let mutable b = Console.ReadLine() |> int
            let out = homework.hw2.fourthEx  array a b
            printf " the result of doing ex: "
            printf "%A" out

        if results.Contains FifthExercise
        then
            printf "enter two elements: "
            let first = Console.ReadLine() |> int
            let second = Console.ReadLine() |> int
            let array = homework.hw2.fifthEx first second
            printf " the result of doing ex: "
            printf "%A" array

        if results.Contains SixthExercise
        then
            printf "enter size of array: "
            let size = Console.ReadLine() |> int
            let array = homework.hw2.makeArray size
            printf "enter i and j indexes: "
            let mutable i = Console.ReadLine() |> int
            let mutable j = Console.ReadLine() |> int
            let out = homework.hw2.sixthEx array i j
            printf " the result of doing ex: "
            printf "%A" out

        else
            parser.PrintUsage() |> printfn "%s"
        0
