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



        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | First_exercise -> "start first exercise"
                | Second_exercise -> "start second exercise"
                | Third_exercise -> "start third exercise"
                | Fourth_exercise -> "start fourth exercise"



    [<EntryPoint>]
    let main (argv: string array) =
        printf "dont enter too big numbers"
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")

        let results = parser.Parse(argv)
        if results.Contains First_exercise then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.first_ex number
            printfn "the result of doing fist ex. = %A" current
        if results.Contains Second_exercise then
            printf "enter the number: "
            let number = Console.ReadLine() |> int
            let current = homework.hw2.second_ex number
            printfn "the result of doing second ex. = %A" current
        if results.Contains Third_exercise then
            printf "enter the size of array: "
            let mutable size = Console.ReadLine()|>int
            if size<1 then
               let mutable flag=true
               while flag do
                    printf "you entered wrong size, try again :)"
                    size <- Console.ReadLine() |> int
                    if size>1 then
                        flag<-false
                    else flag<-true
            printf "enter max elem: "
            let max = Console.ReadLine()|>int
            let array=homework.hw2.make_array size
            homework.hw2.third_ex array size max
        if results.Contains Fourth_exercise then
            printf "enter the size of array: "
            let mutable size = Console.ReadLine()|>int
            if size<1 then
               let mutable flag=true
               while flag do
                    printf "you entered wrong size, try again :) "
                    size <- Console.ReadLine() |> int
                    if size>1 then
                        flag<-false
                    else flag<-true
            let array=homework.hw2.make_array size
            printf"enter range (from): "     // для удобства лучше будет упорядочить(пусть точка - не диапазон)
            let mutable a =Console.ReadLine()|>int
            printf"enter range (to): "
            let mutable b = Console.ReadLine()|>int
            let mutable right=0
            let mutable left=0

            if a=b then

                let mutable flag=true
                while flag do
                    printf "incorrect input!!!"

                    a<-Console.ReadLine()|>int
                    b<-Console.ReadLine()|>int
                    if not (a=b)then
                        flag<-false

            elif a>b then
                right<-a
                left<-b


            else
                right<-b
                left<-a




            homework.hw2.fourth_ex array left right








        else
            parser.PrintUsage() |> printfn "%s"
        0
