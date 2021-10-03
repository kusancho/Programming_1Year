module Message

type Message =
    | EOS of AsyncReplyChannel<unit>

    | Go of AsyncReplyChannel<unit>

    | Tuple of int [][] * int [][]
