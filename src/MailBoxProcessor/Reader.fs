module Reader


/// in: path to directory 
/// out: names of files in directory
let getFilesName path = System.IO.Directory.GetFiles(path) |> List.ofArray
