type grammar = Grammar.grammar

let (>>=) = Result.bind

let parse lexbuf =
  try Ok (Parse_bnf.parse lexbuf (Bnf_parser.Incremental.grammar lexbuf.lex_curr_p))
  with Util.Syntax_error (pos, err) ->
    match pos with
    | Some (line, pos) -> Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
    | None -> Error (Printf.sprintf "Syntax error: %s" err)

let load_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf >>= Grammar.check_grammar

let load_from_file filename =
  let ic = open_in filename in
  load_from_channel ic

let load_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf >>= Grammar.check_grammar

let dump_rules = Grammar.string_of_rules

let reduce_symbol = Grammar.reduce_symbol

let depth_exceeded max_depth depth =
  match max_depth with
  | None -> false
  | Some max_depth -> depth > max_depth

let rec _generate ?(debug=ignore) ?(max_depth=None) ?(max_non_productive=None) ?(symbols=[Grammar.Nonterminal "start"]) ?(separator="") ?(callback=ignore) grammar depth nonprod_depth =
  if depth_exceeded max_depth depth then Error ("Maximum total number of reductions exceeded") else
  let out, syms = reduce_symbol ~debug:debug symbols grammar in
  match out with
  | None ->
    if syms = [] then Ok () else
    if depth_exceeded max_non_productive nonprod_depth then Error ("Maximum number of non-productive reductions exceeded") else
    _generate ~debug:debug ~max_depth:max_depth ~max_non_productive:max_non_productive
      ~symbols:syms ~separator:separator ~callback:callback
      grammar (depth + 1) (nonprod_depth + 1)
  | Some str ->
    let () = callback str in
    let() = callback separator in
    if syms = [] then Ok () else
    _generate ~debug:debug ~max_depth:max_depth ~max_non_productive:max_non_productive
      ~symbols:syms ~separator:separator ~callback:callback
      grammar (depth + 1) 0

let generate ?(debug=ignore) ?(max_depth=None) ?(max_non_productive=None) ?(start_symbol="start") ?(separator="") ?(callback=ignore) grammar =
  try _generate ~debug:debug ~max_depth:max_depth ~max_non_productive:max_non_productive
    ~symbols:([Grammar.Nonterminal start_symbol]) ~separator:separator ~callback:callback grammar 0 0
  with Grammar.Reduction_error e -> Error e
