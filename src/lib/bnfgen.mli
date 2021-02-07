module Grammar : sig
  exception Grammar_error of string

  type symbol =
    | Terminal of string
    | Nonterminal of string
    | Repeat of symbol * (int * int)

  type rule_alternative = { weight: int; symbols: symbol list }
  type rule = string * (rule_alternative list)
  type grammar = rule list

  val reduce_symbol :
    ?debug:bool -> ?debug_fun:(string -> unit) ->
    symbol list -> grammar -> (string option * symbol list)
end

val grammar_from_string : string -> (Grammar.grammar, string) result
val grammar_from_channel : in_channel -> (Grammar.grammar, string) result
val grammar_from_file : string -> (Grammar.grammar, string) result

val grammar_to_string : Grammar.grammar -> string

val generate :
  ?dump_stack:bool ->
  ?debug:bool ->
  ?debug_fun:(string -> unit) ->
  ?max_depth:int option ->
  ?max_non_productive:int option ->
  ?start_symbol:string ->
  ?separator:string ->
  ?callback:(string -> unit) -> Grammar.grammar -> (unit, string) result

