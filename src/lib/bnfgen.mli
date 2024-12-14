module Grammar : sig
  exception Grammar_error of string

  type symbol =
    | Terminal of (string * bool)
    | Nonterminal of (string * bool)
    | Repeat of symbol * (int * int)
    | SymbolMarker of (string * bool)

  type rule_alternative = { weight: int; symbols: symbol list }
  type rule = string * (rule_alternative list)
  type grammar = rule list

  val reduce_symbol :
    ?debug:bool -> ?debug_fun:(string -> unit) -> ?separator:string ->
    symbol list -> grammar -> (string option * symbol list)
end

type settings = {
  dump_stack: bool;
  debug: bool;
  debug_fun: (string -> unit);
  max_reductions : int option;
  max_nonproductive_reductions : int option;
  symbol_separator: string;
}

val default_settings : settings

val grammar_from_string : string -> (Grammar.grammar, string) result
val grammar_from_channel : in_channel -> (Grammar.grammar, string) result
val grammar_from_file : string -> (Grammar.grammar, string) result

val grammar_to_string : Grammar.grammar -> string

val check_grammar : Grammar.grammar -> (unit, string) result
val check_grammar_exn : Grammar.grammar -> unit

val generate : ?settings:settings -> (string -> unit) -> Grammar.grammar -> (string * bool) -> (unit, string) result

val generate_string : ?settings:settings -> Grammar.grammar -> (string * bool) -> (string, string) result
