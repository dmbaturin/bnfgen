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
  | Grammar.Grammar_error msg -> Error (Printf.sprintf "Malformed grammar: %s" msg)

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

let rec _generate ?(dump_stack=false) ?(debug=false) ?(debug_fun=print_endline) ?(max_depth=None) ?(max_non_productive=None) ?(symbols=[Grammar.Nonterminal "start"]) ?(separator="") ?(callback=ignore) grammar depth nonprod_depth =
  let () = if dump_stack then debug_fun @@ Printf.sprintf "Symbol stack: %s\n" (List.map Grammar.string_of_symbol symbols |> String.concat " ") in
  if depth_exceeded max_depth depth then Error ("Maximum total number of reductions exceeded") else
  let out, syms = Grammar.reduce_symbol ~debug:debug symbols grammar in
  match out with
  | None ->
    if syms = [] then Ok () else
    if depth_exceeded max_non_productive nonprod_depth then Error ("Maximum number of non-productive reductions exceeded") else
    _generate ~dump_stack:dump_stack ~debug:debug ~debug_fun:debug_fun ~max_depth:max_depth ~max_non_productive:max_non_productive
      ~symbols:syms ~separator:separator ~callback:callback
      grammar (depth + 1) (nonprod_depth + 1)
  | Some str ->
    let () = callback str in
    let() = callback separator in
    if syms = [] then Ok () else
    _generate ~dump_stack:dump_stack ~debug:debug ~max_depth:max_depth ~max_non_productive:max_non_productive
      ~symbols:syms ~separator:separator ~callback:callback
      grammar (depth + 1) 0

let generate ?(dump_stack=false) ?(debug=false) ?(debug_fun=print_endline) ?(max_depth=None) ?(max_non_productive=None) ?(start_symbol="start") ?(separator="") ?(callback=ignore) grammar =
  try _generate ~dump_stack:dump_stack ~debug:debug ~debug_fun:debug_fun ~max_depth:max_depth ~max_non_productive:max_non_productive
    ~symbols:([Grammar.Nonterminal start_symbol]) ~separator:separator ~callback:callback grammar 0 0
  with Grammar.Grammar_error e -> Error e
