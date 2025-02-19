module Grammar : sig
  exception Grammar_error of string

type symbol_flags = {
    (** The symbol itself is marked sticky in the grammar. *)
    sticky: bool;

    (** The symbol must be rendered as sticky
       because it was produced during reduction of a sticky parent.

       The reason not to use the normal sticky flag
       is to be able to render rules for debugging purposes
       exactly as they were in the source grammar.

       The reason why we need that flag propagation at all
       is that symbol reduction is tail-recursive
       to support infinite depth and output size,
       so there is no other way to determine if we need
       to emit a separator or not.
       In a naively recursive implementation,
       we could just check for stickyness before return,
       but in a tail-recursive implementation,
       we have to pass that state around.
     *)
    sticky_parent: bool;
  }

  type symbol =
    | Terminal of (string * symbol_flags)
    | Nonterminal of (string * symbol_flags)
    | Repeat of symbol * (int * int)

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

val generate : ?settings:settings -> (string -> unit) -> Grammar.grammar -> string -> (unit, string) result

val generate_string : ?settings:settings -> Grammar.grammar -> string -> (string, string) result
