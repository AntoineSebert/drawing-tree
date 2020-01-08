// Learn more about F# at http://fsharp.org

//namespace DrawingTree

module main

open AST
open DrawingTree.ASTConverter
open DrawingTree.Test

[<EntryPoint>]
let main argv =

    let ast = P(test_declarations, [PrintLn(B(true))])
    let generic_tree = convert ast

    0
