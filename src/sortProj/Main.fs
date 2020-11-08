namespace homework

open homework

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
                | FirstExercise -> "start first exercise from fourth homework"
                | SecondExercise -> "start second exercise from fourth homework"
                | ThirdExercise -> "start third exercise from fourth homework"
                | FourthExercise -> "start fourth exercise from fourth homework"
                | FifthExercise -> "start fifth exercise from fourth homework"
                | SixthExercise -> "start sixth exercise from fourth homework"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        let killerOfCopyPaste exercise =
            printf "enter path: "
            let file = Console.ReadLine()
            exercise file

        if results.Contains FirstExercise
        then killerOfCopyPaste fourthHomework.firstEx

        elif results.Contains SecondExercise
        then killerOfCopyPaste fourthHomework.secondEx

        elif results.Contains ThirdExercise
        then killerOfCopyPaste fourthHomework.thirdEx

        elif results.Contains FourthExercise
        then killerOfCopyPaste fourthHomework.fourthEx

        elif results.Contains FifthExercise
        then
            printfn "enter 2 number: "
            let array = Array.zeroCreate 2
            for i = 0 to array.Length - 1 do array.[i] <- Console.ReadLine() |> int32
            fourthHomework.fifthEx array

        elif results.Contains SixthExercise
        then
            printfn "enter 4 numbers: "
            let array = Array.zeroCreate 4
            for i = 0 to array.Length - 1 do array.[i] <- Console.ReadLine() |> int16
            fourthHomework.sixthEx array

        else
            parser.PrintUsage() |> printfn "%s"
        0
