(*
 * Copyright (c) 2014, 2019 Daniil Baturin
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

type rule_part = { weight: int; symbols: symbol list }

type rule = { lhs: string; rhs: rule_part list }

type grammar = rule list

exception Reduction_error of string


(* Anything to string, mainly for parser debug *)

let string_of_symbol s =
    match s with
    | Terminal s -> Printf.sprintf "\"%s\"" s
    | Nonterminal s -> Printf.sprintf "<%s>" s

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

let rec reduce_rhs ?(debug=false) buffer rhs grammar delimiter maxdepth depth =
    match rhs with
    | [] -> ()
    | hd :: tl ->
        match hd with
        | Terminal hd ->
          Buffer.add_string buffer hd;
          Buffer.add_string buffer delimiter;
          reduce_rhs buffer tl grammar delimiter maxdepth depth
        | Nonterminal hd ->
            reduce_symbol ~debug:debug buffer hd grammar delimiter maxdepth (depth + 1);
            reduce_rhs ~debug:debug buffer tl grammar delimiter maxdepth depth
and reduce_symbol ?(debug=false) buffer name grammar delimiter maxdepth depth =
    let () = if debug then Printf.eprintf "Reducing symbol <%s>\n" name in
    let r = find_production name grammar in
    match r with
    | None ->
        (* Shouldn't happen since grammar is validated at load time *)
        raise (Reduction_error (Printf.sprintf "Undefined symbol <%s>" name))
    | Some rhs ->
        if (depth_exceeded maxdepth depth) then raise (Reduction_error "Maximum recursion depth exceeded") else
        let symbols = pick_element rhs in
        let () = if debug then Printf.eprintf "Alternative taken: %s\n" (string_of_rule_rhs_part {weight=1; symbols=symbols}) in
        reduce_rhs ~debug:debug buffer symbols grammar delimiter maxdepth depth

let reduce ?(debug=false) ?(max_depth=None) ?(start_symbol="start") ?(separator="") grammar =
    let buffer = Buffer.create 16 in
    try
      let () = reduce_symbol ~debug:debug buffer start_symbol grammar separator max_depth 0 in
      Ok (Buffer.contents buffer)
    with Reduction_error msg -> Error msg
