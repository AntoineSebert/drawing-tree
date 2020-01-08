module DrawingTree.FastConcat

let (++) (left : System.Text.StringBuilder) (right : 't) : System.Text.StringBuilder =
    left.Append right

let (+=) (left : System.Text.StringBuilder) (right : 't) : unit =
    left ++ right |> ignore

let f_concat(first: string, second: string): string =
    let builder = new System.Text.StringBuilder()

    builder
    ++ first
    ++ second

    builder.ToString()