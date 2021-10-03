module MailBoxProcessorTests


open Expecto
open Generator
open Config


let generatorConfig = GeneratorConfig(Int, 10, 10, 0.5, 10, __SOURCE_DIRECTORY__ + "/Matrices")
let rightConfig = Config(All, (__SOURCE_DIRECTORY__ + "/Matrices"), 2, 10, Config.makeDefaultMethodMap 0.1 0.3)
let wrongConfig = Config(Amount 11, (__SOURCE_DIRECTORY__ + "/Matrices"), 2, 10, Config.makeDefaultMethodMap 0.1 0.3)

generateMatrix <| generatorConfig


[<Tests>]
let testTree =
    testList "mailbox" [

//        testCase "with wrong config" <| fun _ ->
//            Expect.throws (fun _ -> Worker.processFiles wrongConfig) "not enough file"


        testCase "with right config" <| fun _ ->
            Expect.equal (Worker.processFiles rightConfig) () ""

]
