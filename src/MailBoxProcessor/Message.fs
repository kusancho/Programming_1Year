module Message


type Message =
    | EOS of AsyncReplyChannel<unit>
    | Go of AsyncReplyChannel<unit>
    | Pair of int [][] * int [][]
