module Loader


open Config
open Message
open writePrint


let loader path (balancer: MailboxProcessor<Message>) (pairAmount: TypeProcessing) =

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
                        (readIntMatrix fst, readIntMatrix snd) |> Tuple |> balancer.Post
                        inbox.Post msg
                        return! loop tail

                    | hd :: tl -> failwith "number of files isn't even"

                | _ -> failwith "not loader's task"
            }

        let mtxList = Reader.getFilesName path

        match pairAmount with
        | All -> loop mtxList

        | Amount pairAmount ->
            if mtxList.Length < pairAmount * 2
            then failwith "not enough files"
            loop mtxList.[ .. pairAmount * 2 - 1]
        )
