namespace sortProj

module Main =
    open Argu
    open System
    type CLIArguments =
        | BubbleArray
        | QuickArray
        | BubbleList
        | QuickList
        | Packing32to64
        | Packing16to64
        | Unpack64to32
        | Unpack64to16

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | BubbleArray -> "start first exercise from fourth homework"
                | QuickArray -> "start second exercise from fourth homework"
                | BubbleList -> "start third exercise from fourth homework"
                | QuickList -> "start fourth exercise from fourth homework"
                | Packing32to64 -> "pack 32 to 64"
                | Packing16to64 -> "pack 16 to 64"
                | Unpack64to32 -> "unpack 64 to 32"
                | Unpack64to16 -> "unpack 64 to 16"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework")
        let results = parser.Parse(argv)

        let template exercise writeCollection =
            printf "enter path: "
            let pathToFile = Console.ReadLine()
            let out = exercise pathToFile
            printfn "sorted collection: %A" out
            printf "enter path of out file: "
            let outPath = Console.ReadLine()
            writeCollection outPath out

        let unpack funName =
            printf "enter number to unpack: "
            printfn "unpacked numbers: %A" (funName (Console.ReadLine() |> int64))

        if results.Contains BubbleArray
        then template fourthHomework.bSortArrayFromFile fourthHomework.outArray

        elif results.Contains QuickArray
        then template fourthHomework.qSortArrayFromFile fourthHomework.outArray

        elif results.Contains BubbleList
        then template fourthHomework.bSortListFromFile fourthHomework.outList

        elif results.Contains QuickList
        then template fourthHomework.qSortListFromFile fourthHomework.outList

        elif results.Contains Packing32to64
        then
            printfn "enter 2 number: "
            let first = Console.ReadLine() |> int32
            let second = Console.ReadLine() |> int32
            printfn "packed numbers: %A" (fourthHomework.pack32To64 first second)

        elif results.Contains Packing16to64
        then
            printfn "enter 4 numbers: "
            let first = Console.ReadLine() |> int16
            let second = Console.ReadLine() |> int16
            let third = Console.ReadLine() |> int16
            let fourth = Console.ReadLine() |> int16
            printfn "packed numbers: %A" (fourthHomework.pack16To64 first second third fourth)

        elif results.Contains Unpack64to32
        then unpack fourthHomework.unpack64To32

        elif results.Contains Unpack64to16
        then unpack fourthHomework.unpack64To16

        else
            parser.PrintUsage() |> printfn "%s"
        0
