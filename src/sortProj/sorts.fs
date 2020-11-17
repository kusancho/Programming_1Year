module sorts

    let swapArr (arr: array<_>) i j =
        let temp = arr.[j]
        arr.[j] <- arr.[i]
        arr.[i] <- temp

    let bubbleSortA (arr: array<_>) =
        if arr.Length = 0 || arr.Length = 1
        then arr
        else
            for i = arr.Length - 1 downto 1 do
                for j = 0 to i - 1 do
                    if arr.[j] > arr.[j + 1] then swapArr arr j (j + 1)
            arr

    let rec hidRealiseQA (arr: array<_>) left right = //qsort for range [l,r]
        if left <> right
        then
            let pivot = arr.[left]
            let mutable border = left
            for i = left to right do
                if pivot > arr.[i]
                then
                    swapArr arr i border
                    border <- border + 1
            if border = left
            then hidRealiseQA arr (left + 1) right
            else
                hidRealiseQA arr left (border - 1)
                hidRealiseQA arr border right

    let quickSortA (arr: array<_>) =
        if arr.Length > 1
        then
            hidRealiseQA arr 0 (arr.Length - 1)
            arr
        else arr

    let rec quickSortL inList =
        let rec split_ (myList: list<_>) (left: list<_>) (right: list<_>) (pivot: list<_>) =
            if not myList.IsEmpty
            then
                if pivot.Head > myList.Head
                then split_ myList.Tail (left @ [myList.Head]) right pivot
                elif pivot.Head = myList.Head
                then split_ myList.Tail left right (pivot @ [myList.Head])
                else split_ myList.Tail left (right @ [myList.Head]) pivot
            else (quickSortL left) @ (quickSortL pivot) @ (quickSortL right)
        if inList.Length > 1
        then
            let out = split_ inList.Tail [] [] [inList.Head]
            out
        else inList

    let bubbleSortL (myList: list<_>) =
        let mutable newList = myList
        let rec bubble (left: list<_>) (middle: list<_>) (right: list<_>) = // in result the biggest element on top of list.
            match right with
            | [] -> left @ middle
            | _ ->
                if middle.Head > right.Head
                then bubble (left @ [right.Head]) middle right.Tail
                elif middle.Head < right.Head
                then bubble (left @ middle) [right.Head] right.Tail
                else bubble left (middle @ [right.Head]) right.Tail
        for i = 1 to newList.Length do
            newList <- bubble [] [newList.Head] newList.Tail
        newList
