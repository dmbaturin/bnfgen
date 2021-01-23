(*
 * Copyright (c) 2021 Daniil Baturin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

type symbol = 
    | Terminal of string 
    | Nonterminal of string
    | Repeat of symbol * (int * int)

type rule_part = { weight: int; symbols: symbol list }

type rule = { lhs: string; rhs: rule_part list }

type grammar = rule list

exception Reduction_error of string


(* Anything to string, mainly for parser debug *)

let rec string_of_symbol s =
    match s with
    | Terminal s -> Printf.sprintf "\"%s\"" s
    | Nonterminal s -> Printf.sprintf "<%s>" s
    | Repeat (s, (min, max)) ->
      if (min = max) then Printf.sprintf "%s{%d}" (string_of_symbol s) min
      else Printf.sprintf "%s{%d-%d}" (string_of_symbol s) min max

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
        Printf.sprintf "%s | %s" (string_of_rule_rhs_part hd) (string_of_rule_rhs tl)

let string_of_rule r =
    Printf.sprintf "%s ::= %s;" (string_of_symbol (Nonterminal r.lhs)) (string_of_rule_rhs r.rhs)

let string_of_rules r =
    let rule_str_list = List.map string_of_rule r in
    String.concat "\n" rule_str_list

(** Rule sanity checking *)
let check_for_nonexistent rs =
    let get_left_hand_sides rs = List.map (fun r -> r.lhs) rs in
    let get_referenced_nonterminals r =
        let symbols = r.rhs |> List.map (fun r -> r.symbols) |> List.concat in
        List.fold_left (fun ss s -> match s with Nonterminal s' -> s' :: ss | _ -> ss) [] symbols
    in
    let check_symbol known_symbols s =
        try ignore @@ List.find ((=) s) known_symbols
        with _ -> failwith (Printf.sprintf "Undefined symbol <%s>" s)
    in
    let rec aux rs known_symbols =
        match rs with
        | [] -> ()
        | r :: rs ->
            let nts = get_referenced_nonterminals r in
            let _ = List.iter (check_symbol known_symbols) nts in
            aux rs known_symbols
    in aux rs (get_left_hand_sides rs)

let rec check_for_duplicates rs =
    let sort_rules rs = List.sort (fun x y -> compare x.lhs y.lhs) rs in
    let rs = sort_rules rs in
    match rs with
    | [] | [_] -> ()
    | r :: r' :: rs ->
        if r.lhs = r'.lhs then failwith (Printf.sprintf "Duplicate definition of symbol <%s>" r.lhs)
        else check_for_duplicates (r' :: rs)

let check_grammar rs =
  try
    let () =
        check_for_duplicates rs;
        check_for_nonexistent rs
    in Ok rs
  with Failure msg -> Error msg


(** Rule reduction *)

(* We support weight for cases in rules with alternation,
   so we need to support weighted random selection here. *)
let total_weight l =
    List.fold_left (fun x y -> x + y.weight) 0 l

let rec weighted_random acc r l =
    match l with
    | [] ->
        (* Shouldn't happen, the parser takes care of it *)
        failwith "Rule with empty right-hand side"
    | [x] -> x.symbols
    | hd :: tl ->
        let acc' = acc + hd.weight in
        if r < acc' then hd.symbols
        else weighted_random acc' r tl

let pick_element l =
    let tw = total_weight l in
    let r = Random.int tw in
    weighted_random 0 r l

let find_production name grammar =
    try
        let rule = List.find (fun x -> x.lhs = name) grammar in
        Some rule.rhs
    with Not_found -> None

let sort_rule_parts l =
    List.sort (fun x y -> compare x.weight y.weight) l



let depth_exceeded maxdepth depth =
    match maxdepth with
    | None -> false
    | Some maxdepth -> depth > maxdepth

let rec reduce_symbol ?(debug=ignore) buffer sym_stack depth max_depth separator grammar =
  match sym_stack with
  | [] -> ()
  | sym :: syms ->
    match sym with
    | Terminal str ->
       Buffer.add_string buffer str;
       Buffer.add_string buffer separator;
       reduce_symbol ~debug:debug buffer syms (depth + 1) max_depth separator grammar
    | Nonterminal name ->
      if (depth_exceeded max_depth depth) then raise (Reduction_error "Maximum recursion depth exceeded") else
      let () = Printf.ksprintf debug "Reducing symbol <%s>" name in
      let rhs = find_production name grammar in
      let rhs =
        if Option.is_some rhs then Option.get rhs
        else raise (Reduction_error (Printf.sprintf "Undefined symbol <%s>" name))
      in
      let new_syms = pick_element rhs in
      let () = Printf.ksprintf debug "Alternative taken: %s" (string_of_rule_rhs_part {weight=1; symbols=new_syms}) in
      let syms = List.append new_syms syms in
      reduce_symbol ~debug:debug buffer syms (depth + 1) max_depth separator grammar
    | Repeat (s, (min, max)) ->
      if (min > max) then raise (Reduction_error (Printf.sprintf "Malformed range {%d,%d} (min > max)" min max)) else
      let times = if (min = max) then min else ((Random.int (max - min)) + min) in
      let new_syms = List.init times (fun _ -> s) in
      let () = Printf.ksprintf debug "Repetition range {%d,%d}, repeating %d times" min max times in
      let syms = List.append new_syms syms in
      reduce_symbol ~debug:debug buffer syms depth max_depth separator grammar

let reduce ?(debug=ignore) ?(max_depth=None) ?(start_symbol="start") ?(separator="") grammar =
    let buffer = Buffer.create 16 in
    try
      let () = reduce_symbol ~debug:debug buffer [Nonterminal start_symbol] 0 max_depth separator grammar in
      Ok (Buffer.contents buffer)
    with Reduction_error msg -> Error msg
