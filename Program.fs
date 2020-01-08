// Learn more about F# at http://fsharp.org

//namespace DrawingTree

module main

open AST
open DrawingTree.Tree
open DrawingTree.ASTConverter
open DrawingTree.PostScriptGenerator
open System.IO

[<EntryPoint>]
let main argv =
    let test_types = [
        VarDec(ITyp, "name_int");
        VarDec(BTyp, "name_bool");
        VarDec(ATyp(ITyp, Some(2)), "name_array");
        VarDec(ATyp(ITyp, None), "name_array");
        VarDec(PTyp(BTyp), "name_pointer");
        VarDec(FTyp([ATyp(ITyp, Some(2)); PTyp(BTyp)], Some(ITyp)), "function_name");
        VarDec(FTyp([ATyp(ITyp, None); PTyp(BTyp)], None), "function_name");
    ]

    let test_function = [
        FunDec(Some(ITyp), "function_name", [VarDec(ITyp, "name_int")], PrintLn(B(true)));
        FunDec(None, "function_name", [VarDec(ITyp, "name_int")], PrintLn(B(true)));
    ]

    let test_declarations = List.append test_types test_function

    let test_expressions = [
        N(-20);
        B(false);
        Access(AVar("variable access"));
        Addr(AVar("variable access"));
        Apply("function_name", [N(20)]);
    ]

    let test_statements = [
        PrintLn(N(25));
        Ass(AVar("variable access"), N(25));
        Return(None);
        Return(Some(N(25)));
        Alt(GC([
            B(true), [PrintLn(N(25))];
            N(25), [PrintLn(N(25))];
        ]));
        Do(GC([
            B(true), [PrintLn(N(25))];
            N(25), [PrintLn(N(25))];
        ]));
        Block([VarDec(ITyp, "name_int")], [PrintLn(N(25))]);
        Call("call", [N(-20)]);
    ]

    let test_guarded_command = GC([
        B(true), [PrintLn(N(25))];
        N(25), [PrintLn(N(25))];
    ])

    let ast = P(test_declarations, [PrintLn(B(true))])
    let generic_tree = convert ast

    let sb = System.Text.StringBuilder()

    let min x y = if x > y then y else x
    let max x y = if x > y then x else y

    let rec getMax tree maxx =
        let rec getMax' children maxx =
            match children with
            | [] -> maxx
            | child::children -> max (getMax child maxx) (getMax' children maxx)
        match tree with
        | Node((_,x),children) -> max x (getMax' children maxx)

    let rec getMin tree minx =
        let rec getMin' children minx =
            match children with
            | [] -> minx
            | child::children -> min (getMin child minx) (getMin' children minx)
        match tree with
        | Node((_,x),children) -> min x (getMin' children minx)

    let rec getDepth tree x =
        let rec getDepth' children x =
            match children with
            | [] -> x
            | child::children -> max (getDepth child x) (getDepth' children x)
        match tree with
        | Node(_,children) -> max x (getDepth' children (x+1))

    // Updating the size of the tree and remembering the previous x values

    let rec updateSize tree size prevx =
        let rec updateSizeChildren children size prevx =
            match children with
            | [] -> []
            | child::children -> updateSize child size prevx::updateSizeChildren children size prevx
        match tree with
        | Node((a,x),children) -> Node((a,(prevx+x)*size),updateSizeChildren children size (x+prevx))

    let initText tree ysize = "%!\n<</PageSize[" + string (200.0+(getMax tree 0.0)-(getMin tree 0.0)) + " " + string ((getDepth tree 0) * ysize) + "]/ImagingBBox null>> setpagedevice\n1 1 scale\n " + string (100.0-(getMin tree 0.0)) + " " + string ((getDepth tree 0) * ysize - 1) + " translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"

    let writeLabel label =  " (" + string label + ")" + " dup stringwidth pop 2 div neg 0 rmoveto show\n"

    let getSize child =
        match child with
        | Node((_,x),_) -> x

    let moveto x y = string x + " " + string y + " moveto\n"
    let lineto x y = string x + " " + string y + " lineto\n"

    let drawLine xs ys xe ye = (moveto xs ys) + (lineto xe ye)

    let drawMiddleLine children y = 
        let rec drawMiddleLine' children y = 
            match children with
            | [] -> "" // No children: No line
            | [Node((_,x),_)] -> lineto x y // Last element: Draw line to
            | Node((_,x),_)::children -> drawMiddleLine' children y // The list is not empty: Go to next child
        match children with
        | [] -> ""
        | [Node((_,x),_)] -> "" // If it is the only element - no need to draw a line
        | Node((_,x),_)::children -> moveto x y + drawMiddleLine' children y
        
    let drawFirstLine x y children =
        match children with
        | [] -> "" // No children: No line
        | _ -> drawLine x (y-10.0) x (y-40.0)

    let rec drawLines tree y =
        match tree with
        | Node((label,x:float),children) -> (moveto x y) + (writeLabel (string label)) + (drawFirstLine x y children) + (drawMiddleLine children (y-40.0)) + (List.fold (fun acc elem -> acc+drawLine (getSize elem) (y-40.0) (getSize elem) (y-80.0)) "" children) + ("stroke\n") + (List.fold (fun acc elem -> acc+drawLines elem (y-90.0)) "" children)

    let generateTree tree = File.WriteAllText("file1.ps", (sb.Append((initText tree 90)).Append(drawLines tree -10.0).Append("showpage").ToString()))

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    generateTree (updateSize (design generic_tree) 100.0 0.0)
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds

    0