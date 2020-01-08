module DrawingTree.ASTConverter

open AST
open Tree

exception SyntaxError of string * string * int

// TODO : generate type names from types

let rec parse_type (token: Typ): Tree<'a> =
    match token with
    | ITyp -> Node("ITyp", [])
    | BTyp -> Node("BTyp", [])
    | ATyp(arr_type, opt_size) ->
        match opt_size with
        | Some(size) -> Node("ATyp", [parse_type arr_type; Node(string size, [])])
        | None -> Node("ATyp", [parse_type arr_type])
    | PTyp(p_type) -> Node("PTyp", [parse_type p_type])
    | FTyp(param_types, opt_return_type) ->
        let param_nodes = [for param_type in param_types do parse_type param_type]
        match opt_return_type with
        | Some(return_type) -> Node("FTyp", (parse_type return_type)::param_nodes)
        | None ->Node("FTyp", param_nodes)

let rec parse_expr(token: Exp): Tree<'a> =
    match token with
    | N(number)                 -> Node("N", [Node(string number, [])])
    | B(boolean)                -> Node("B", [Node(string boolean, [])])
    | Access(access)            -> Node("Access", [parse_access access])
    | Addr(access)              -> Node("Addr", [parse_access access])
    | Apply(name, expressions)  -> Node(" Apply", [Node(name, []); for expr in expressions do parse_expr expr])

and parse_access(token: Access): Tree<'a> =
    match token with
    | AVar(name)            -> Node("AVar", [Node(name, [])])
    | AIndex(access, expr)  -> Node("AIndex", [parse_access access; parse_expr expr])
    | ADeref(expr)          -> Node("ADeref", [parse_expr expr])

let rec parse_gc(token: GuardedCommand): Tree<'a> =
    let parse_gc'(expr: Exp, statements: Stm list) =
        (parse_expr expr)::List.map parse_stmt statements

    Node("GuardedCommand", [
        match token with
        //| []        -> raise(SyntaxError("GuardedCommand:", "Excepected ...", 0))
        | GC(elements)  -> Node("GC", [for element in elements do parse_expr (fst element); for element in elements do (for stmt in snd element do parse_stmt stmt)])
    ])

/// Parse a Statement token into a Node.
/// @param      token   a statement
/// @returns    a generic Tree representing a statement
and parse_stmt (token: Stm): Tree<'a> =
    match token with
    | PrintLn(expr)                     -> Node("PrintLn", [parse_expr expr])
    | Ass(access, expr)                 -> Node("Ass", [parse_access access; parse_expr expr])
    | Return(opt_expr)                  -> Node("Return", [
        match opt_expr with
        | Some(expr) -> parse_expr expr
        | None -> ()
    ])
    | Alt(gc)                           -> Node("Alt", [parse_gc gc])
    | Do(gc)                            -> Node("Do", [parse_gc gc])
    | Block(declarations, statements)   -> Node("PrintLn", [for decl in declarations do parse_decl decl; for stmt in statements do parse_stmt stmt])
    | _                                 -> raise(SyntaxError("Statement error:", "Excepected a VarDec or FunDec.", 0))

/// Parse a Declaration into a Node.
/// @param      token   a declaration
/// @returns    a generic Tree representing a declaration
and parse_decl (token: Dec): Tree<'a> =
    match token with
    | VarDec(var_type, name)                    -> Node("VarDec", [parse_type var_type; Node(name, [])])
    | FunDec(opt_type, name, parameters, body)  ->
        match opt_type with
        | Some(return_type) -> Node("FunDec", [parse_type return_type; Node(name, []); for param in parameters do parse_decl param; parse_stmt body])
        | None              -> Node("FunDec", [Node(name, []); for param in parameters do parse_decl param; parse_stmt body])
    | _                                         -> raise(SyntaxError("Declaration error:", "Excepected a VarDec or FunDec.", 0))

/// Module Entry point.
///
/// @param      program The AST to convert
/// @returns    a generic Tree representing a program
let convert (program: Program): Tree<'a> =
    let tree =
        match program with
        | P(declarations, statements) ->
            Node("Program", [for dec in declarations do parse_decl dec; for stmt in statements do parse_stmt stmt])

    tree