module MatrixGeneratorMain


open Generator
open Argu
open System.IO


type CliArguments =
    | Lines of lines: int
    | Cols of cols: int
    | Amount of amount: int
    | Sparsity of sparsity: float
    | Path of path: string
    | Type of dataType: DataType

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Lines _ -> "Specify the number of rows"
            | Cols _ -> "Specify the number of cols"
            | Amount _ -> "Specify the number of matrices"
            | Sparsity _ -> "Specify the sparsity as number from 0.0 to 1.0 (percentage of neutral elements)"
            | Path _ -> "Specify the target directory"
            | Type _ -> "Specify the type of matrices (int, float, bool)"

[<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CliArguments> (programName = "Generator")
        let errorFunc (str: string) =
            parser.PrintUsage() |> ignore
            failwith <| sprintf "%s" str
        try
            let args = parser.Parse argv
            if args.GetResult Lines <= 0 || args.GetResult Cols <= 0
            then errorFunc "Number of rows and cols must be positive"
            elif args.GetResult Amount <= 0
            then errorFunc "Number of matrices must be positive"
            elif args.GetResult Sparsity < 0.0 || args.GetResult Sparsity > 1.0
            then errorFunc "The sparsity should be defined as a number between 0.0 and 1.0"
            elif not (Directory.Exists (args.GetResult Path))
            then errorFunc "The specified path does not exist"
            else
                GeneratorConfig
                            (args.GetResult Type, args.GetResult Lines,
                             args.GetResult Cols, args.GetResult Sparsity,
                             args.GetResult Amount, args.GetResult Path)
                |> generateMatrix
            0
        with e ->
            printfn "%s" e.Message
            1
