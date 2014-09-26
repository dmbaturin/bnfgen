type symbol = Terminal of string | Nonterminal of string
type rule_part = Rule_part of int * symbol list
type rule_rhs = Rule_rhs of rule_part list
type rule = Rule of string * rule_rhs
type rules = rule list
type grammar = rules
val string_of_symbol : symbol -> string
val string_of_rule_rhs_part : rule_part -> string
val string_of_rule_rhs : rule_part list -> string
val string_of_rule : rule -> string
val string_of_rules : rule list -> string
val total_weight : rule_part list -> int
val weighted_random : int -> int -> rule_part list -> symbol list
val pick_element : rule_part list -> symbol list
val has_element : 'a -> 'a list -> bool
val find_production : string -> rule list -> rule_rhs option
val sort_rule_parts : rule_part list -> rule_part list
val reduce_rhs : symbol list -> rule list -> string -> string
val reduce : string -> rule list -> string -> string
