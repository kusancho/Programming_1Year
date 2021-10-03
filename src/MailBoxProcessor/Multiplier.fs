module Multiplier


open Message

/// helper prepare _ [][] to arg types of multiplyFun
let multiplier multiplyFun helper =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    ch.Reply()
                    return! loop ()

                | Tuple (fst, snd) ->
                    let res = multiplyFun (helper fst) (helper snd)
                    return! loop ()

                | _ -> failwith "not multiplier's task"
            }
        loop ()
    )
