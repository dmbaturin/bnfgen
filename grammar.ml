type symbol = 
    | Terminal of string 
    | Nonterminal of string

type rule = Rule of string * (symbol list list)

type rule_rhs = Rule_rhs of symbol list list

type rule_part = Rule_part of symbol list

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
        if lhs = name then Some (Rule_rhs rhs)
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
    | None -> failwith "Rule not found"
    | Some Rule_rhs rhs ->
        reduce_rhs (pick_element rhs) grammar
