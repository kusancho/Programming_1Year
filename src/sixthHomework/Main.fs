namespace homework
// ПРЕДПОЛАГАЕТСЯ, ЧТО ПОЛЬЗОВАТЕЛЬ НЕ БУДЕТ ВВОДИТЬ СЛИШКОМ БОЛЬШИХ ЧИСЕЛ
module Main =
    open Argu
    open System
    type CLIArguments =
        | FirstExercise1

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FirstExercise1 -> "start first exercise from second homework"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)
        if true
        then
            let mymatrix = myMatrix.readMyMatrix <| Console.ReadLine()
            printf "enter path to out: "
            myMatrix.outMyMatrix mymatrix <| Console.ReadLine()

        else
            parser.PrintUsage() |> printfn "%s"
        0
