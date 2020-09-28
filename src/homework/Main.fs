namespace homework
// ПРЕДПОЛАГАЕТСЯ, ЧТО ПОЛЬЗОВАТЕЛЬ НЕ БУДЕТ ВВОДИТЬ СЛИШКОМ БОЛЬШИХ ЧИСЕЛ
module Main =
    open Argu
    open System
    type CLIArguments =
        | First_exercise
        | Second_exercise
        | Third_exercise
        | Fourth_exercise
        | Fifth_exercise
        | Sixth_exercise

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | First_exercise -> "start first exercise"
                | Second_exercise -> "start second exercise"
                | Third_exercise -> "start third exercise"
                | Fourth_exercise -> "start fourth exercise"
                | Fifth_exercise -> "start fifth exercise"
                | Sixth_exercise -> "start sixth exercise"

    [<EntryPoint>]
    let main (argv: string array) =
        printfn "dont enter too big numbers"
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        if results.Contains First_exercise
        then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.firstEx  number
            printfn "the result of doing fist ex. = %A" current

        if results.Contains Second_exercise
        then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.secondEx  number
            printfn "the result of doing second ex. = %A" current

        if results.Contains Third_exercise
        then
            printf "enter the size of array: "
            let size = Console.ReadLine()|>int
            printf "enter max elem: "
            let max = Console.ReadLine() |> int
            let array = homework.hw2.make_array  size
            let out = homework.hw2.thirdEx  array size max
            if out.Length = 0
            then printf "doesn't exest "
            else
                for i = 0 to size - 1 do
                                printf "%A " out.[i]

        if results.Contains Fourth_exercise
        then
            printf "enter the size of array: "
            let mutable size = Console.ReadLine() |> int
            if size < 1
            then
               let mutable flag = true
               while flag do
                    printf "you entered wrong size, try again :) "
                    size <- Console.ReadLine() |> int
                    if size > 1
                    then
                        flag <- false
                    else flag <- true
            let array = homework.hw2.make_array size
            printf"enter range (from): "     // для удобства лучше будет упорядочить(пусть точка - не диапазон)
            let mutable a = Console.ReadLine() |> int
            printf"enter range (to): "
            let mutable b = Console.ReadLine() |> int
            let out = homework.hw2.fourthEx  array a b
            printf " the result of doing ex: "
            if out = [|6;6;6|]
            then
                printf "wrong input (size) indexes doesn't exist"
            else
                for i = 0 to out.Length - 1 do
                    printf "%A " out.[i]

        if results.Contains Fifth_exercise
        then
            printf "enter two elements: "
            let first = Console.ReadLine() |> int
            let second = Console.ReadLine() |> int
            let array = homework.hw2.fifthEx first second
            printf "%A" array

        if results.Contains Sixth_exercise
        then
            printf "enter size of array: "
            let size = Console.ReadLine() |> int
            let array = homework.hw2.make_array size
            printf "enter i and j indexes: "
            let mutable i = Console.ReadLine() |> int
            let mutable j = Console.ReadLine() |> int
            let out = homework.hw2.sixthEx array i j
            if out = [|6;6;6|]
            then
                printf " indexes doesn't exist"
            else
                printf "%A" out

        else
            parser.PrintUsage() |> printfn "%s"
        0
