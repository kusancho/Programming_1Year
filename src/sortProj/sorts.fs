namespace homework

open System.Globalization

module sorts =

    let swapArr (array: byref<array<int>>) i j =
        array.[i] <- array.[j] + array.[i]
        array.[j] <- array.[i] - array.[j]
        array.[i] <- array.[i] - array.[j]

    let bubbleSortA (array: array<int>) =
        if array.Length = 0 || array.Length = 1
        then array
        else
            for i = array.Length - 1 downto 1 do
                for j = 0 to i - 1 do
                    if array.[j] > array.[j + 1] then swapArr &array j (j + 1)
            array

    let rec hidRealiseQA (array: byref<array<int>>) (left: int) (right: int) = //qsort for range [l,r]
        if not (left = right)
        then
            let pivot = array.[left]
            let mutable border = left
            for i = left to right do
                if pivot > array.[i]
                then
                    swapArr &array i border   //swap without extra memory
                    border <- border + 1
            if border = left
            then hidRealiseQA &array (left + 1) right
            else
                hidRealiseQA &array left (border - 1)
                hidRealiseQA &array border right

    let quickSortA (array: byref<array<int>>) =
        hidRealiseQA &array 0 (array.Length - 1)

    let rec quickSortL (inList: list<int>) =
        let rec split_ (list: list<int>) (left: list<int>) (right: list<int>) (pivot: list<int>) =
            if not list.IsEmpty
            then
                if pivot.Head > list.Head
                then split_ list.Tail (left @ [list.Head]) right pivot
                elif pivot.Head = list.Head
                then split_ list.Tail left right (pivot @ [list.Head])
                else split_ list.Tail left (right @ [list.Head]) pivot
            else ((quickSortL left) @ (quickSortL pivot)) @ (quickSortL right)
        if inList.Length > 1
        then
            let out = split_ inList.Tail [] [] [inList.Head]
            out
        else inList

    let rec bubble (left: list<int>) (middle: list<int>) (right: list<int>) = // in result the biggest element on top of list.
        match right with
        | [] -> left @ middle
        | _ ->
            if middle.Head > right.Head
            then bubble (left @ [right.Head]) middle right.Tail
            elif middle.Head < right.Head
            then bubble (left @ middle) [right.Head] right.Tail
            else bubble left (middle @ [right.Head]) right.Tail

    let bubbleSortL (list: list<int>) =
        let mutable newList = list
        for i = 1 to newList.Length do
            newList <- bubble [] [newList.Head] newList.Tail
        newList
