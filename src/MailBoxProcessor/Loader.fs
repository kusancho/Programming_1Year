module Loader


open Config
open Message
open writePrint


let getFilesName path = System.IO.Directory.GetFiles(path) |> List.ofArray


let loader path (balancer: MailboxProcessor<Message>) (pairAmount: TypeProcessing) =

    let mtxList = getFilesName path

    let input =
        match pairAmount with
        | All -> mtxList

        | Amount pairAmount ->
            if mtxList.Length < pairAmount * 2
            then failwith "not enough files"
            else mtxList.[ .. pairAmount * 2 - 1]

    MailboxProcessor.Start(fun inbox ->
        let rec loop input =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    balancer.PostAndReply EOS
                    ch.Reply()

                | Go ch as msg ->
                    match input with
                    | [] ->
                        inbox.Post (EOS ch)
                        return! loop input

                    | fst :: snd :: tail ->
                        (readIntMatrix fst, readIntMatrix snd) |> Pair |> balancer.Post
                        inbox.Post msg
                        return! loop tail

                    | hd :: tl -> failwith "number of files isn't even"

                | _ -> failwith "not loader's task"
            }
        loop input
        )
