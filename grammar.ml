type symbol = 
    | Terminal of string 
    | Nonterminal of string

type rule = Rule of string * (symbol list list)

type rules = rule list

type grammar = rules


(* Anything to string, mainly for parser debug *)

let string_of_symbol s =
    match s with
    | Terminal s -> "\"" ^ s ^ "\""  (* "terminal" *)
    | Nonterminal s -> "<" ^ s ^ ">" (* <nonterminal> *)

let string_of_rule_rhs_part r =
    let l = List.map string_of_symbol r in
    String.concat " " l

let rec string_of_rule_rhs r =
    match r with
    | [] -> ""
    | [hd] -> string_of_rule_rhs_part hd
    | hd :: tl ->
        (string_of_rule_rhs_part hd) ^ " | " ^ (string_of_rule_rhs tl)

let string_of_rule r =
    let Rule (lhs, rhs) = r in
    (string_of_symbol (Nonterminal lhs)) ^ " ::= " ^ (string_of_rule_rhs rhs)

let rec string_of_rules r =
    let rule_str_list = List.map string_of_rule r in
    String.concat "\n" rule_str_list
