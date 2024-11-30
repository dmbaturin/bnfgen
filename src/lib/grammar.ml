(*
 * Copyright (c) 2024 Daniil Baturin
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

exception Grammar_error of string

type symbol =
  | Terminal of string
  | Nonterminal of string
  | Repeat of symbol * (int * int)
  | SymbolMarker of string

type rule_alternative = { weight: int; symbols: symbol list }
type rule = string * (rule_alternative list)
type grammar = rule list

let grammar_error s = raise (Grammar_error s)

(* Anything to string, mainly for parser debug *)

let rec string_of_symbol s =
  match s with
  | SymbolMarker name -> Printf.sprintf "(SymbolMarker <%s>)" name
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
  let (name, alts) = r in
  Printf.sprintf "%s ::= %s;" (string_of_symbol (Nonterminal name)) (string_of_rule_rhs alts)

let to_string r =
  let rule_str_list = List.map string_of_rule r in
  String.concat "\n" rule_str_list

(* Rule sanity checking.

   Making illegal states unrepresentable would require dependent types,
   so we cannot avoid runtime checks.

   The parser takes care of some but not all of these errors,
   so it's a good idea to run check_grammar on all grammars.
 *)
let check_for_nonexistent rs =
  let get_left_hand_sides rs = List.map (fun (name, _) -> name) rs in
  let get_referenced_nonterminals r =
    let (_, alts) = r in
    let symbols = alts |> List.map (fun r -> r.symbols) |> List.concat in
      List.fold_left (fun ss s -> match s with Nonterminal s' -> s' :: ss | _ -> ss) [] symbols
  in
  let check_symbol known_symbols s =
    try ignore @@ List.find ((=) s) known_symbols
    with _ -> Printf.ksprintf grammar_error "Undefined symbol <%s>" s
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
  let sort_rules rs = List.sort (fun (x, _)  (y, _) -> compare x y) rs in
  let rs = sort_rules rs in
  match rs with
  | [] | [_] -> ()
  | r :: r' :: rs ->
    let (name, _) = r in
    let (name', _) = r' in
    if name = name' then Printf.ksprintf grammar_error "Duplicate definition of symbol <%s>" name
    else check_for_duplicates (r' :: rs)

let rec check_for_empty_rules rs =
  match rs with
  | [] -> ()
  | (name, []) :: _ -> Printf.ksprintf grammar_error "Empty rule for symbol <%s>" name
  | _ :: rs -> check_for_empty_rules rs

let check_repeats rs =
  let check_rule_repeats (_, r) =
    let syms = List.map (fun x -> x.symbols) r |> List.concat in
    List.iter (fun s ->
      match s with
      | Repeat (_, (min, max)) ->
        if (min > max) then Printf.ksprintf grammar_error "Malformed range {%d,%d} (min > max)" min max
      | _ -> ()) syms
  in List.iter check_rule_repeats rs

let check_grammar rs =
  check_for_empty_rules rs;
  check_for_duplicates rs;
  check_for_nonexistent rs;
  check_repeats rs

(** Rule reduction *)

(* We support weight for cases in rules with alternation,
   so we need to support weighted random selection here. *)
let total_weight l =
  List.fold_left (fun x y -> x + y.weight) 0 l

let rec weighted_random acc r l =
  match l with
  | [] ->
    Printf.ksprintf grammar_error "Rule with empty right-hand side"
  | [x] -> x.symbols
  | hd :: tl ->
    let acc' = acc + hd.weight in
    if r < acc' then hd.symbols
    else weighted_random acc' r tl

let pick_element l =
  let tw = total_weight l in
  let r = Random.int tw in
  weighted_random 0 r l

let find_production name grammar = List.assoc_opt name grammar

let sort_rule_parts l =
  List.sort (fun x y -> compare x.weight y.weight) l

let reduce_symbol ?(debug=false) ?(debug_fun=print_endline) sym_stack grammar =
  match sym_stack with
  | [] -> (None, [])
  | sym :: syms ->
    match sym with
    | SymbolMarker name ->
      let () = if debug then Printf.ksprintf debug_fun "Finished resolving symbol \"%s\"" name in
      (None, syms)
    | Terminal t ->
      let () = if debug then Printf.ksprintf debug_fun "Emitting terminal \"%s\"" t in
      (Some t, syms)
    | Nonterminal name ->
      let () = if debug then Printf.ksprintf debug_fun "Reducing symbol <%s>" name in
      let rhs = find_production name grammar in
      let rhs =
        if Option.is_some rhs then Option.get rhs
        else Printf.ksprintf grammar_error "Undefined symbol <%s>" name
      in
      let new_syms = pick_element rhs in
      let () =
        if debug then Printf.ksprintf debug_fun "Alternative taken: %s" (string_of_rule_rhs_part {weight=1; symbols=new_syms})
      in
      let new_syms = List.append new_syms [(SymbolMarker name)] in
      let syms = List.append new_syms syms in
      (None, syms)
    | Repeat (s, (min, max)) ->
      if (min > max) then Printf.ksprintf grammar_error "Malformed range {%d,%d} (min > max)" min max else
      let times = if (min = max) then min else ((Random.int (max - min)) + min) in
      let new_syms = List.init times (fun _ -> s) in
      let () = if debug then Printf.ksprintf debug_fun "Repetition range {%d,%d}, repeating %d times" min max times in
      let syms = List.append new_syms syms in
      (None, syms)
