module Balancer


open Config
open Message


let tupleRate fst snd = (MatrixGeneratorTests.evaluateSparsity fst, MatrixGeneratorTests.evaluateSparsity snd)


let balancer (qtMultiply: MailboxProcessor<Message>) (qtParallelMultiply: MailboxProcessor<Message>)
             (arrMultiply: MailboxProcessor<Message>) (arrParallelMultiply: MailboxProcessor<Message>)
             (map: float * float -> Method)  =

    let eos = qtMultiply.PostAndReply EOS, arrMultiply.PostAndReply EOS,
              qtParallelMultiply.PostAndReply EOS, arrParallelMultiply.PostAndReply EOS

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    eos |> ignore
                    ch.Reply()

                | Tuple (fst, snd) as tuple ->
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
