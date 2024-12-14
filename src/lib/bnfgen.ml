module Grammar = Grammar

let parse lexbuf =
  try
    let grammar = Parse_bnf.parse lexbuf (Bnf_parser.Incremental.grammar lexbuf.lex_curr_p) in
    let () = Grammar.check_grammar grammar in
    Ok grammar
  with
  | Util.Syntax_error (pos, err) ->
    begin
      match pos with
      | Some (line, pos) ->
        Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
      | None -> Error (Printf.sprintf "Syntax error: %s" err)
    end
  | Grammar.Grammar_error msg -> Error (Printf.sprintf "Grammar error: %s" msg)

let grammar_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf

let grammar_from_file filename =
  let ic = open_in filename in
  let g = grammar_from_channel ic in
  let () = close_in ic in
  g

let grammar_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf

let grammar_to_string = Grammar.to_string

let depth_exceeded max_depth depth =
  match max_depth with
  | None -> false
  | Some max_depth -> depth > max_depth

let check_grammar g =
  try
    Grammar.check_grammar g;
    Ok ()
  with Grammar.Grammar_error msg ->
    Error msg

let check_grammar_exn g = Grammar.check_grammar g

type settings = {
  dump_stack: bool;
  debug: bool;
  debug_fun: (string -> unit);
  max_reductions : int option;
  max_nonproductive_reductions : int option;
  symbol_separator: string;
}

let default_settings = {
  dump_stack = false;
  debug = false;
  debug_fun = print_endline;
  max_reductions = None;
  max_nonproductive_reductions = None;
  symbol_separator = ""
}

let generate ?(settings=default_settings) callback grammar start_symbol =
  let rec aux settings callback grammar reductions nonprod_reductions sym_stack =
    let () =
      if settings.dump_stack then begin
        let syms_str = List.map Grammar.string_of_symbol sym_stack |> String.concat " " |> Printf.sprintf "Symbol stack: %s" in
        settings.debug_fun syms_str
      end
    in
    if depth_exceeded settings.max_reductions reductions then Error ("Maximum total number of reductions exceeded") else
    let output, sym_stack =
      Grammar.reduce_symbol ~debug:settings.debug ~debug_fun:settings.debug_fun
        ~separator:settings.symbol_separator sym_stack grammar in
    match output with
    | None ->
      if sym_stack = [] then Ok () else
      if depth_exceeded settings.max_nonproductive_reductions nonprod_reductions
      then Error ("Maximum number of non-productive reductions exceeded")
      else aux settings callback grammar (reductions + 1) (nonprod_reductions + 1) sym_stack
    | Some str ->
      let () = callback str in
      if sym_stack = [] then Ok ()
      else aux settings callback grammar (reductions + 1) 0 sym_stack
  in
  try aux settings callback grammar 0 0 [Grammar.Nonterminal start_symbol]
  with Grammar.Grammar_error e -> Error e

let generate_string ?(settings=default_settings) grammar start_symbol =
  let (>>=) = Result.bind in
  let buf = Buffer.create 4096 in
  let res = generate ~settings:settings (Buffer.add_string buf) grammar start_symbol in
  res >>= (fun () -> Ok (Buffer.contents buf))
