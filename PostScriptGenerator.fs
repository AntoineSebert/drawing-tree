module DrawingTree.PostScriptGenerator

open Tree
open System.IO

let rec updateSize tree size prevx =
    let rec updateSizeChildren children size prevx =
        match children with
        | [] -> []
        | child::children -> updateSize child size prevx::updateSizeChildren children size prevx
    match tree with
    | Node((a,x),children) -> Node((a,(prevx+x)*size),updateSizeChildren children size (x+prevx))

// Synthesizing to PostScript

let initText = "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"

let writeLabel label =  " (" + string label + ")" + " dup stringwidth pop 2 div neg 0 rmoveto show\n"

// Draw a line from the middle of the height
// The line should be the length of the 
// let rec lineWidth children = 
//     match children with
//     | [] -> []
//     | Node((_,x),_)::children -> (int x)::drawLine children

let getSize child =
    match child with
    | Node((_,x),_) -> x

// let rec adjustWidthToMiddle line =
//     match line with
//     | [] -> []
//     | h::line -> (h-(getSize line)/2)::adjustWidthToMiddle line size

let moveto x y = string x + " " + string y + " moveto\n"
let lineto x y = string x + " " + string y + " lineto\n"

let drawLine xs ys xe ye = (moveto xs ys) + (lineto xe ye)

let drawMiddleLine children y = 
    let rec drawMiddleLine' children y = 
        match children with
        | [] -> "" // No children: No line
        | Node((_,x),_)::[] -> lineto x y // Last element: Draw line to
        | Node((_,x),_)::children -> drawMiddleLine' children y // The list is not empty: Go to next child
    match children with
    | [] -> ""
    | Node((_,x),_)::[] -> "" // If it is the only element - no need to draw a line
    | Node((_,x),_)::children -> moveto x y + drawMiddleLine' children y
    
let drawFirstLine x y children =
    match children with
    | [] -> "" // No children: No line
    | _ -> drawLine x (y-10.0) x (y-40.0)


let rec drawLines tree y =
    match tree with
    | Node((label,x:float),children) -> (moveto x y) + (writeLabel (string label)) + (drawFirstLine x y children) + (drawMiddleLine children (y-40.0)) + (List.fold (fun acc elem -> acc+drawLine (getSize elem) (y-40.0) (getSize elem) (y-80.0)) "" children) + "stroke\n" + List.fold (fun acc elem -> acc+drawLines elem (y-90.0)) "" children

let generateTree tree = File.WriteAllText("file1.ps", (initText + (drawLines tree -10.0) + "showpage"))

let timer = new System.Diagnostics.Stopwatch()
timer.Start()
generateTree (updateSize (design testTree) 100.0 0.0)
printfn "Elapsed Time: %i" timer.ElapsedMilliseconds;;