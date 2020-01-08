module DrawingTree.ASTConverter

open AST
open Tree

exception SyntaxError of string * string * int

// TODO : generate type names from types

/// Parse a Type token into a Node.
///
/// @param      token   a type
/// @returns    a generic Tree
let rec parse_type (token: Typ): Tree<string> =
    match token with
    | ITyp | BTyp                           -> Node(token.GetType().Name, [])
    | ATyp(arr_type, opt_size)              -> Node("ATyp", [parse_type arr_type;
        match opt_size with
        | Some(size)    -> Node(string size, [])
        | None          -> ()
    ])
    | PTyp(p_type)                          -> Node("PTyp", [parse_type p_type])
    | FTyp(param_types, opt_return_type)    -> Node("FTyp", [(for param_type in param_types do parse_type param_type);
        match opt_return_type with
        | Some(return_type) -> parse_type return_type
        | None              -> ()
    ])

/// Parse an Expression token into a Node.
///
/// @param      token   an expression
/// @returns    a generic Tree
let rec parse_expr(token: Exp): Tree<string> =
    match token with
    | N(number)                 -> Node("N", [Node(string number, [])])
    | B(boolean)                -> Node("B", [Node(string boolean, [])])
    | Access(access)            -> Node("Access", [parse_access access])
    | Addr(access)              -> Node("Addr", [parse_access access])
    | Apply(name, expressions)  -> Node("Apply", Node(name, [])::(List.map parse_expr expressions))

/// Parse an Access token into a Node.
///
/// @param      token   an access
/// @returns    a generic Tree
and parse_access(token: Access): Tree<string> =
    match token with
    | AVar(name)            -> Node("AVar", [Node(name, [])])
    | AIndex(access, expr)  -> Node("AIndex", [parse_access access; parse_expr expr])
    | ADeref(expr)          -> Node("ADeref", [parse_expr expr])

/// Parse a GuardedCommand token into a Node.
///
/// @param      token   a guarded command
/// @returns    a generic Tree
let rec parse_gc(token: GuardedCommand): Tree<string> =
    Node("GuardedCommand", [
        match token with
        | GC([])        -> raise(SyntaxError("GuardedCommand:", "Excepected ...", 0))
        | GC(elements)  -> Node("GC", [for element in elements do parse_expr (fst element); for element in elements do (for stmt in snd element do parse_stmt stmt)])
    ])

/// Parse a Statement token into a Node.
///
/// @param      token   a statement
/// @returns    a generic Tree
and parse_stmt (token: Stm): Tree<string> =
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
    | Block(declarations, statements)   -> Node("Block", List.append (List.map parse_decl declarations) (List.map parse_stmt statements))
    | Call(name, expressions)   -> Node("Call", Node(name, [])::(List.map parse_expr expressions))
    | _                                 -> raise(SyntaxError("Statement error:", "Excepected a VarDec or FunDec.", 0))

/// Parse a Declaration into a Node.
///
/// @param      token   a declaration
/// @returns    a generic Tree
and parse_decl (token: Dec): Tree<string> =
    match token with
    | VarDec(var_type, name)                    -> Node("VarDec", [parse_type var_type; Node(name, [])])
    | FunDec(opt_type, name, parameters, body)  -> Node("FunDec", [
        match opt_type with
        | Some(return_type) -> parse_type return_type
        | None              -> ();
        Node(name, []); for param in parameters do parse_decl param; parse_stmt body
    ])

/// Module Entry point.
///
/// @param      program The AST to convert
/// @returns    a generic Tree
let convert (program: Program): Tree<string> =
    let tree =
        match program with
        | P(declarations, statements) ->
            Node("test", List.append (List.map parse_decl declarations) (List.map parse_stmt statements))

    tree