module DrawingTree.Tree

type 'a Tree = Node of 'a * ('a Tree list)
type Extent = (float*float) list

let movetree (Node((label, x), subtrees), y : float) = Node((label, x + y), subtrees)

let moveextent (e : Extent, x) = List.map (fun e -> fst e + x, snd e + x) e

let rec merge ps qs =
    match ps, qs with
    | [], qs -> qs
    | ps, [] -> ps
    | (p, _)::ps, (_, q)::qs    -> (p, q) :: merge ps qs

let mergelist es = List.fold merge [] es

let rmax p q = if p > q then p else q

let rec fit ps qs =
    match ps, qs with
    | (_, p)::ps, (q, _)::qs    -> rmax (fit ps qs) (p - q + 1.0)
    | _, _                      -> 0.0

let fitlistl es =
    let rec fitlistl' acc l =
        match l with
        | []        -> []
        | (e::l)    ->
            let x = fit acc e
            in x :: fitlistl' (merge acc (moveextent (e,x))) l
    in fitlistl' [] es

let fitlistr es =
    let rec fitlistr' acc l =
        match l with
        | []        -> []
        | (e::l)    ->
            let x = -(fit e acc)
            in  x::fitlistr' (merge (moveextent (e, x)) acc) l
    in List.rev (fitlistr' [] (List.rev es))

let mean (x,y) = (x + y) / 2.0

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es));;

let design tree =
    let rec design' (Node(label, subtrees)) =
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions        = fitlist extents
        let ptrees           = List.map movetree (List.zip trees positions)
        let pextents         = List.map moveextent (List.zip extents positions)
        let resultextent     = (0.0, 0.0)::mergelist pextents
        let resulttree       = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst (design' tree)

let rec reflect (Node(v, subtrees)) = Node(v, List.map reflect (List.rev subtrees))

let rec reflectpos (Node((v, x : float), subtrees)) = Node((v, -x), List.map reflectpos subtrees)