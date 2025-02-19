(*
 * Copyright (c) 2025 Daniil Baturin
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

type symbol_flags = {
  (* The symbol itself is marked sticky in the grammar. *)
  sticky: bool;

  (* The symbol must be rendered as sticky
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

let default_flags = {sticky=false; sticky_parent=false}

let grammar_error s = raise (Grammar_error s)

(* Anything to string, mainly for parser debug *)

let sticky_flag sym_data =
  (* Only render the sticky flag if it was originally there
     and was not added during reduction of a sticky parent. *)
  if sym_data.sticky then "~"
  else ""

let rec string_of_symbol s =
  match s with
  | Terminal (content, sym_data) -> Printf.sprintf "\"%s\"%s" content (sticky_flag sym_data)
  | Nonterminal (name, sym_data) -> Printf.sprintf "<%s>%s" name (sticky_flag sym_data)
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
  Printf.sprintf "%s ::= %s;" (string_of_symbol (Nonterminal (name, default_flags))) (string_of_rule_rhs alts)

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
      List.fold_left (fun ss s -> match s with Nonterminal (s', _) -> s' :: ss | _ -> ss) [] symbols
  in
  let check_symbol known_symbols name =
    try ignore @@ List.find ((=) name) known_symbols
    with _ -> Printf.ksprintf grammar_error "Undefined symbol <%s>" name
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

let rec make_sticky sym =
  match sym with
  | Terminal (content, sym_flags) -> Terminal (content, {sym_flags with sticky_parent=true})
  | Nonterminal (name, sym_flags) -> Nonterminal (name, {sym_flags with sticky_parent=true})
  | Repeat (sym, num) -> Repeat ((make_sticky sym), num)

let is_sticky sym_data =
  sym_data.sticky || sym_data.sticky_parent

let reduce_symbol ?(debug=false) ?(debug_fun=print_endline) ?(separator="") sym_stack grammar =
  match sym_stack with
  | [] -> (None, [])
  | sym :: syms ->
    match sym with
    | Terminal (content, sym_data) ->
      let () = if debug then Printf.ksprintf debug_fun {|Emitting terminal "%s"|} content in
      let output =
        if ((is_sticky sym_data) || (separator = "")) then content
        else content ^ separator
      in
      (Some output, syms)
    | Nonterminal (name, sym_data) ->
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
      let new_syms =
        (* If the current symbol is not sticky,
           we just push everything it produced on the stack and proceed.

           But if it's sticky, we need to make sure not to emit the separator
           when we emit the last terminal that was produced from that sticky parent.
           So we mark the last symbol in the list with [sticky_parent] flag
           to allow that marker to eventually propagate down to the last terminal.
         *)
        if not (is_sticky sym_data) then new_syms
        else begin
          let rev_new_syms = List.rev new_syms in
          let last_sym = List.hd rev_new_syms in
          let last_sym = make_sticky last_sym in
          last_sym :: List.tl rev_new_syms |> List.rev
        end
      in
      let syms = List.append new_syms syms in
      (None, syms)
    | Repeat (s, (min, max)) ->
      if (min > max) then Printf.ksprintf grammar_error "Malformed range {%d,%d} (min > max)" min max else
      (* The argument of [Random.int] is an exclusive boundary,
         while BNFGen ranges are inclusive.
         We compensate for that by always adding +1 to the boundary. *)
      let times = if (min = max) then min else ((Random.int (max - min + 1)) + min) in
      let new_syms = List.init times (fun _ -> s) in
      let () = if debug then Printf.ksprintf debug_fun "Repetition range {%d,%d}, repeating %d times" min max times in
      let syms = List.append new_syms syms in
      (None, syms)
