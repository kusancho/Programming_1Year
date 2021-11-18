module Balancer


open Config
open Message
open writePrint


let tupleRate fst snd = (evaluateSparsity fst, evaluateSparsity snd)


let balancer (qtMultiply: MailboxProcessor<Message>) (qtParallelMultiply: MailboxProcessor<Message>)
             (arrMultiply: MailboxProcessor<Message>) (arrParallelMultiply: MailboxProcessor<Message>)
             (map: float * float -> Method)  =

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    qtMultiply.PostAndReply EOS
                    arrMultiply.PostAndReply EOS
                    arrParallelMultiply.PostAndReply EOS
                    qtParallelMultiply.PostAndReply EOS
                    ch.Reply()

                | Pair (fst, snd) as tuple ->
                    match  map <| tupleRate fst snd with
                        | QtParallel -> qtParallelMultiply.Post tuple
                        | QtDefault -> qtMultiply.Post tuple
                        | ArrDefault -> arrMultiply.Post tuple
                        | ArrParallel -> arrParallelMultiply.Post tuple
                    return! loop ()

                | _ -> failwith "not balancer's task"
            }
        loop ()
    )
