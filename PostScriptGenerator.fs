module DrawingTree.PostScriptGenerator

open Tree
open System.IO

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
    | Node(((_,_),x),_) -> x

// let rec adjustWidthToMiddle line =
//     match line with
//     | [] -> []
//     | h::line -> (h-(getSize line)/2)::adjustWidthToMiddle line size

let moveto x y = string x + " " + string y + " moveto\n"
let lineto x y = string x + " " + string y + " lineto\n"

let drawLine xs ys xe ye = (moveto xs ys) + (lineto xe ye)

let drawMiddleLine children y xsize = 
    let rec drawMiddleLine' children y xsize = 
        match children with
        | [] -> "" // No children: No line
        | Node(((_,_),x),_)::[] -> lineto (x*xsize) y // Last element: Draw line to
        | Node(((_,_),x),_)::children -> drawMiddleLine' children y xsize // The list is not empty: Go to next child
    match children with
    | [] -> ""
    | Node(((_,_),x),_)::children -> moveto (x*xsize) y + drawMiddleLine' children y xsize
    
let drawFirstLine x y xsize children =
    match children with
    | [] -> "" // No children: No line
    | _ -> drawLine (x*xsize) (y-10.0) (x*xsize) (y-40.0)


let rec drawLines tree y xsize =
    match tree with
    | Node(((label:string,_),x:float),children) -> (moveto (x*xsize) y) + (writeLabel label) + (drawFirstLine x y xsize children) + (drawMiddleLine children (y-40.0) xsize) + (List.fold (fun acc elem -> acc+drawLine ((getSize elem)*xsize) (y-40.0) ((getSize elem)*xsize) (y-80.0)) "" children) + "stroke\n" + List.fold (fun acc elem -> acc+drawLines elem (y-90.0) xsize) "" children

let synth tree xsize = File.WriteAllText("file.ps", (initText + (drawLines tree -10.0 xsize) + "showpage"))

let testTree = Node(("P",0.0),[Node(("A",0.0),[Node(("A1",0.0),[]);Node(("A2",0.0),[]);Node(("A3",0.0),[])]);Node(("B",0.0),[Node(("B1",0.0),[])]);Node(("C",0.0),[]);Node(("D",0.0),[]);Node(("E",0.0),[]);Node(("F",0.0),[]);Node(("G",0.0),[])])

synth (design testTree 200.0);;