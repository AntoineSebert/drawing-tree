// Learn more about F# at http://fsharp.org

//namespace DrawingTree

module main

open AST
open DrawingTree.ASTConverter
open DrawingTree.Test

let fit_label(label: string, font_size: int): string =
    if 20 < label.Length then
        (label.Substring 17) + "..."
    else
        label

[<EntryPoint>]
let main argv =
    let ast = P(test_declarations, [PrintLn(B(true))])
    let generic_tree = convert ast

    let result = test_fast_concatenation
    printfn "%f:%f" (fst result) (snd result)

    0
