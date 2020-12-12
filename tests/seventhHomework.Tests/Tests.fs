module Tests

open Expecto
open MyList
open MyTree
open MyString

let makeList size =
    let rand = System.Random()
    if size < 2 then [for _ in -10..abs(size) -> rand.Next(1, 2000)]
    else [for _ in 0..size - 1 -> rand.Next(1, 2000)]

let testPropTemplate funcMyList funcList msg =
    testProperty (sprintf "%A" msg) <| fun (size: int) ->
        let lst = makeList size
        let myLst = funcMyList <| MyList.ofList lst
        Expect.equal (MyList.toList myLst) (funcList lst)

[<Tests>]
let testPropertyForMyList =
    testList "tests of MyList functions" [
        // functions ofList/toList tests, when testPropTemplate works

        testPropTemplate MyList.rev List.rev "reverse test"
        testPropTemplate sortMyList List.sort "sort test"

        testProperty "map test" <| fun (size: int) ->
            let lst = makeList size
            let myLst = MyList.ofList lst
            let func = (fun x -> x + 1)
            Expect.equal (List.map func lst) (MyList.toList (MyList.map func myLst))

        testProperty "concat test" <| fun (size: int) ->
            let lst1 = makeList size
            let lst2 = makeList size
            let myLst1 = MyList.ofList lst1
            let myLst2 = MyList.ofList lst2
            Expect.equal (MyList.toList (MyList.concat myLst1 myLst2)) (lst1 @ lst2) "concat doesn't work"

        testProperty "fold test" <| fun (size: int) ->
            let lst = makeList size
            let myLst = MyList.ofList lst
            let func = (fun acc x -> acc + x)
            Expect.equal (MyList.fold func 0 myLst) (List.fold func 0 lst ) "fold doesn't work"

        testProperty "length test" <| fun (size: int) ->
            let lst = makeList size
            let myLst = MyList.ofList lst
            Expect.equal myLst.Length lst.Length "length doesn't work"

        testProperty "head test" <| fun (size: int) ->
            let lst = makeList size
            let myLst = MyList.ofList lst
            Expect.equal myLst.Head lst.Head "head doesn't work"

        testProperty "tail test" <| fun (size: int) ->
            let lst = makeList size
            let myLst = MyList.ofList lst
            Expect.equal (MyList.toList myLst.Tail) lst.Tail "tail doesn't work"
    ]

[<Tests>]
let testsForString =
    testList "MyString test" [
        testProperty "ofStr/toStr/concat" <| fun (size: int) ->
            let str1 = List.fold (fun acc x -> acc + x) "" (List.map (fun x -> string x) (makeList size))
            let str2 = List.fold (fun acc x -> acc + x) "" (List.map (fun x -> string x) (makeList (size * 2)))
            let myStr1 = strToMyStr str1
            let myStr2 = strToMyStr str2
            Expect.equal (myStrToStr <| concatMyStr myStr1 myStr2) (str1 + str2) "MyString doesn't work"
    ]

let testTree = Node (3, Cons ( Node(5, Last (Leaf 3)) , Last (Leaf 4)))

[<Tests>]
let testsForTree =
    testList "MyTree test" [

        testCase "average value" <| fun _ ->
            Expect.equal (averageValInTree testTree) (float(3 + 5 + 3 + 4) / float 4) "average doesn't work"

        testCase "max value" <| fun _ ->
            Expect.equal (maxInTree testTree) 5 "maxInTree doesn't work"

        testCase "fold" <| fun _ ->
            Expect.equal (MyTree.fold (fun acc x -> acc + x) 0 testTree) 15 "fold doesn't work"
    ]
