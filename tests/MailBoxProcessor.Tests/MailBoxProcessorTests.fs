module MailBoxProcessorTests


open Expecto
open Generator
open Config


let generatorConfig = GeneratorConfig(Int, 10, 10, 0.5, 10, __SOURCE_DIRECTORY__ + "/InMatrices")
let rightConfig = Config(All, (__SOURCE_DIRECTORY__ + "/InMatrices"), (__SOURCE_DIRECTORY__ + "/OutMatrices"), 2, 10, Config.makeDefaultMethodMap 0.1)
let wrongConfig = Config(Amount 11, (__SOURCE_DIRECTORY__ + "/InMatrices"), (__SOURCE_DIRECTORY__ + "/OutMatrices"), 2, 10, Config.makeDefaultMethodMap 0.1)

generateMatrix <| generatorConfig


[<Tests>]
let testTree =
    testList "mailbox" [

        testCase "with wrong config" <| fun _ ->
            Expect.throws (fun _ -> Worker.processFiles wrongConfig) "not enough file"


        testCase "with right config" <| fun _ ->
            Expect.equal (Worker.processFiles rightConfig) () ""


        testCase "right amount of output files" <| fun _ ->
            Worker.processFiles rightConfig
            Expect.equal (System.IO.Directory.GetFiles(rightConfig.OutPath)).Length 5 "wrong amount of output files"

]
