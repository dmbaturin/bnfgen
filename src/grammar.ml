type symbol = 
    | Terminal of string 
    | Nonterminal of string

type rule_part = { weight: int; symbols: symbol list }

type rule = { lhs: string; rhs: rule_part list }

type grammar = rule list


(* Anything to string, mainly for parser debug *)

let string_of_symbol s =
    match s with
    | Terminal s -> "\"" ^ s ^ "\""  (* "terminal" *)
    | Nonterminal s -> "<" ^ s ^ ">" (* <nonterminal> *)

let string_of_rule_rhs_part r =
    let l = List.map string_of_symbol r.symbols in
    let sym_str = String.concat " " l in
    let weight_str = if r.weight = 1 then "" else string_of_int r.weight in
    String.concat " " [weight_str; sym_str]

let rec string_of_rule_rhs r =
    match r with
    | [] -> ""
    | [hd] -> string_of_rule_rhs_part hd
    | hd :: tl ->
        (string_of_rule_rhs_part hd) ^ " | " ^ (string_of_rule_rhs tl)

let string_of_rule r =
    (string_of_symbol (Nonterminal r.lhs)) ^ " ::= " ^ (string_of_rule_rhs r.rhs)

let rec string_of_rules r =
    let rule_str_list = List.map string_of_rule r in
    String.concat "\n" rule_str_list

(** Rule reduction *)

(* We support weight for cases in rules with alternation.
   So we need to support weighted random selection here. *)
let total_weight l =
    List.fold_left (fun x y -> x + y.weight) 0 l

let rec weighted_random acc r l =
    match l with
    | [] -> failwith "Cannot select anything from empty list"
    | [x] -> x.symbols
    | hd :: tl ->
        let acc' = acc + hd.weight in
        if r < acc' then hd.symbols
        else weighted_random acc' r tl

let pick_element l =
    let tw = total_weight l in
    let r = Random.int tw in (* print_endline (string_of_int r); *)
    weighted_random 0 r l

let has_element x l =
    List.exists (fun a -> a = x) l

let rec find_production name grammar =
    match grammar with
    | [] -> None
    | hd :: tl ->
        if hd.lhs = name then Some hd.rhs
        else find_production name tl

let sort_rule_parts l =
    List.sort (fun x y -> compare x.weight y.weight) l

let rec reduce_rhs rhs grammar delimiter =
    match rhs with
    | [] -> ""
    | hd :: tl ->
        match hd with
        | Terminal hd -> hd ^ delimiter ^ (reduce_rhs tl grammar delimiter)
        | Nonterminal hd ->
            (reduce hd grammar delimiter) ^ (reduce_rhs tl grammar delimiter)
and reduce name grammar delimiter =
    let r = find_production name grammar in
    match r with
    | None -> failwith ("Rule " ^ name ^ " not found")
    | Some rhs ->
        let symbols = pick_element rhs in
        reduce_rhs symbols grammar delimiter
