type grammar = Grammar.grammar

let (>>=) r f =
    match r with
    | Ok r' -> f r'
    | Error _ as e -> e

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

(* let generate = Grammar.reduce *)

let dump_rules = Grammar.string_of_rules 

let generate = Grammar.reduce
