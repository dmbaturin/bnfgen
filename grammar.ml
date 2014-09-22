type symbol = 
    | Terminal of string 
    | Nonterminal of string

type rule_part = Rule_part of int * (symbol list)

type rule_rhs = Rule_rhs of rule_part list

type rule = Rule of string * rule_rhs

type rules = rule list

type grammar = rules


(* Anything to string, mainly for parser debug *)

let string_of_symbol s =
    match s with
    | Terminal s -> "\"" ^ s ^ "\""  (* "terminal" *)
    | Nonterminal s -> "<" ^ s ^ ">" (* <nonterminal> *)

let string_of_rule_rhs_part r =
    let Rule_part (weight, symbols) = r in
    let l = List.map string_of_symbol symbols in
    let sym_str = String.concat " " l in
    let weight_str = if weight = 1 then "" else string_of_int weight in
    String.concat " " [weight_str; sym_str]

let rec string_of_rule_rhs r =
    match r with
    | [] -> ""
    | [hd] -> string_of_rule_rhs_part hd
    | hd :: tl ->
        (string_of_rule_rhs_part hd) ^ " | " ^ (string_of_rule_rhs tl)

let string_of_rule r =
    let Rule (lhs, rhs) = r in
    let Rule_rhs rhs_symbols = rhs in
    (string_of_symbol (Nonterminal lhs)) ^ " ::= " ^ (string_of_rule_rhs rhs_symbols)

let rec string_of_rules r =
    let rule_str_list = List.map string_of_rule r in
    String.concat "\n" rule_str_list

(* Rule reduction *)

let pick_element l =
    let num = Random.int (List.length l) in
    List.nth l num

let has_element x l =
    List.exists (fun a -> a = x) l

let rec find_production name grammar =
    match grammar with
    | [] -> None
    | hd :: tl ->
        let Rule (lhs, rhs) = hd in
        if lhs = name then Some rhs
        else find_production name tl

let rec reduce_rhs rhs grammar =
    match rhs with
    | [] -> ""
    | hd :: tl ->
        match hd with
        | Terminal hd -> hd ^ " " ^ (reduce_rhs tl grammar)
        | Nonterminal hd ->
            (reduce hd grammar) ^ (reduce_rhs tl grammar)
and reduce name grammar =
    let r = find_production name grammar in
    match r with
    | None -> failwith ("Rule " ^ name ^ " not found")
    | Some Rule_rhs rhs ->
        let Rule_part (weight, symbols) = pick_element rhs in
        reduce_rhs symbols grammar
