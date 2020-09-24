namespace homework
open System
module hw2 =

    let first_ex x =  x*x*x*x + x*x*x + x*x + x + (1)|>int
    let  second_ex x =
       let sqr_x=(x*x)|>int
       let res=(sqr_x*(sqr_x+x+1)+x+1)|>int
       res
    let make_array size = // make random array
        let random_array amount =
            let rand = System.Random()
            Array.init amount (fun _ -> rand.Next ())
        let my_array=random_array size
        my_array
    let third_ex (array: int array) size max =
        let mutable flag =false
        printf "indexes of elements which are less than entered number:"
        for i=0 to size-1 do

            if array.[i]<=max then
                printf "%A " i
                flag<-true
        if(flag) then printf "elements doesn't exist"
    let third_ex_test (array: int array) size max = // function specially for test, because of returning obj
        printf "indexes of elements which are less than entered number:"
        let mutable j=0
        let temp_array = Array.zeroCreate size


        for i=0 to size-1 do

            if array.[i]<=max then
                printf "%A " i
                temp_array.[j]<-i
                j<-j+1
        let out_array=Array.zeroCreate j
        for l=0 to j-1 do
            out_array.[l]<-temp_array.[l]
        out_array
    let fourth_ex (array: int array) left right =
        for i=0 to array.Length-1 do
            if (array.[i]<left) || (array.[i]>right) then
                printfn "%A" i
    let fourth_ex_test (array: int array) left right =
        printf "indexes of elements which are out range:"
        let mutable j=0
        let temp_array = Array.zeroCreate array.Length


        for i=0 to array.Length-1 do

            if array.[i]<left || array.[i]>right then
                printf "%A " i
                temp_array.[j]<-i
                j<-j+1
        let out_array=Array.zeroCreate j
        for l=0 to j-1 do
            out_array.[l]<-temp_array.[l]
        out_array
    let fifth_ex_test (array:int array)  =

            array.[0]<-array.[1]+array.[0]
            array.[1]<-array.[0]-array.[1]
            array.[0]<-array.[0]-array.[1]
            array
    let sixth_ex_test (array:int array) i j=







                array.[i]<-array.[j]+array.[i]
                array.[j]<-array.[i]-array.[j]
                array.[i]<-array.[i]-array.[j]
                array

















