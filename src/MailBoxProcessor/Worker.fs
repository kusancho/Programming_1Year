module Worker


open Config
open Balancer
open Multiplier
open MultiplyFunctions
open Message

let processFiles (config: Config) =
    let balancer = balancer
                       (multiplier quadTreeMultiply quadTreeHelper)
                       (multiplier (quadTreeParallelMultiply config.QuadTreeParallelDepth) quadTreeHelper)
                       (multiplier arrMultiply id)
                       (multiplier (arrMultiplyParallel config.ArrayParallelDepth) id)
                       config.MethodMap

    let mtxLoader = Loader.loader config.Path balancer config.Amount

    mtxLoader.PostAndReply Go
