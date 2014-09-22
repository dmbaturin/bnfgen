open Grammar


let filename = Sys.argv.(1)

let () = Random.self_init()

let main () =
    let input = open_in filename in
    let filebuf = Lexing.from_channel input in
    try
        let g = Parser.grammar Lexer.token filebuf in
        (* Stub, just dump the rules for now *)
        print_endline (string_of_rules g);
        print_endline (reduce "start" g)
    with
    | Lexer.Error msg ->
        Printf.eprintf "%s%!" msg
    | Parser.Error  ->
        Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
    ;
    close_in input

let () = main ()
