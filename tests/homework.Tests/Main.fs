module ExpectoTemplate

open Expecto

let config = { FsCheckConfig.defaultConfig with maxTest = 5 }

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv
