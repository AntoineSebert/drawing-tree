module DrawingTree.Test

open AST

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

open FastConcat

let test_fast_concatenation =
    let timer_concat = System.Diagnostics.Stopwatch.StartNew()

    let mutable test_string = ""
    for i = 0 to 10000 do
        test_string <- test_string + "test,"

    timer_concat.Stop()

    let timer_fast_concat = System.Diagnostics.Stopwatch.StartNew()

    let builder = new System.Text.StringBuilder()
    for i=0 to 10000 do builder ++"test," |> ignore
    builder.ToString()

    timer_fast_concat.Stop()

    timer_concat.Elapsed.TotalMilliseconds, timer_fast_concat.Elapsed.TotalMilliseconds