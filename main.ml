open Grammar

type cmd_action = Dump | Reduce | Help

let () = Random.self_init()

let () = let filename = ref "" and
    separator = ref " " and
    start_symbol = ref "start" and
    action = ref Reduce in
    let args = [
        ("--dump-rules", Arg.Unit (fun () -> action := Dump),
         "Dump production rules and exit");
        ("--separator", Arg.String (fun s -> separator := s),
         "<string>    Token separator for generated output, default is space");
        ("--start", Arg.String (fun s -> start_symbol := s),
        "<string>    Start symbol, default is \"start\"");
    ] in let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTIONS] <BNF file>" in
    if Array.length Sys.argv = 1 then
        begin
            Arg.usage args usage;
            exit 1
        end
    else
        begin
            Arg.parse args (fun f -> filename := f) usage;
            let input = open_in !filename in
            let filebuf = Lexing.from_channel input in
            try
                let g = Parser.grammar Lexer.token filebuf in
                if !action = Dump then print_endline (string_of_rules g)
               else print_endline (reduce !start_symbol g !separator)
            with
            | Lexer.Error msg ->
                Printf.eprintf "%s%!" msg
            | Parser.Error  ->
                Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
            ;
            close_in input
        end
