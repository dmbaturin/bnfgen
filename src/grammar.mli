exception Reduction_error of string

type symbol = Terminal of string | Nonterminal of string | Repeat of symbol * (int * int)
type rule_part = { weight : int; symbols : symbol list; }
type rule = { lhs : string; rhs : rule_part list; }
type grammar = rule list
val string_of_rules : rule list -> string
val sort_rule_parts : rule_part list -> rule_part list
val check_grammar : grammar -> (grammar, string) result
val reduce_symbol : ?debug:(string -> unit) -> symbol list -> grammar -> (string option) * (symbol list)
