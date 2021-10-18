module Worker


open Config
open Balancer
open Multiplier
open MultiplyFunctions
open Message
open QuadTree


let deconverter (x: extendedTree<int>) = x.toArray 0


let processFiles (config: Config) =
    let balancer = balancer
                       (multiplier quadTreeMultiply quadTreeHelper deconverter config.OutPath)
                       (multiplier (quadTreeParallelMultiply config.QuadTreeParallelDepth) quadTreeHelper deconverter config.OutPath)
                       (multiplier arrMultiply id id config.OutPath)
                       (multiplier (arrMultiplyParallel config.ArrayParallelDepth) id id config.OutPath)
                       config.MethodMap

    let mtxLoader = Loader.loader config.InPath balancer config.Amount

    mtxLoader.PostAndReply Go
