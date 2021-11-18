module Multiplier


open Message
open writePrint


let toSeq2D mtx =
    seq <| Array.map (fun arr -> seq <| Array.map (fun elem -> elem.ToString()) arr) mtx


/// helper prepare _ [][] to arg types of multiplyFun
let multiplier multiplyFun converter deconverter path =
    MailboxProcessor.Start(fun inbox ->
        let rec loop counter =
            async {
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    ch.Reply()
                    return! loop counter

                | Pair (fst, snd) ->
                    let res = multiplyFun (converter fst) (converter snd)
                    printMatrix (toSeq2D <| deconverter res) <| path + sprintf "/Matrix%A.txt" counter
                    return! loop <| counter + 1

                | _ -> failwith "not multiplier's task"
            }
        loop 0
    )
