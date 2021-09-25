module MatrixBuilder


open Interfaces
open QuadTree


type ImplementingIMatrix<'t> =
    | QuadTree


let matrixBuilder matrixType lineSize colSize func  =
    match matrixType with
    | QuadTree ->
        extendedTree.init lineSize colSize func :> IMatrix<_>
