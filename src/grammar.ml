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
    Printf.sprintf "%s ::= %s" (string_of_symbol (Nonterminal r.lhs)) (string_of_rule_rhs r.rhs)

let string_of_rules r =
    let rule_str_list = List.map string_of_rule r in
    String.concat "\n" rule_str_list

(** Rule reduction *)

(* We support weight for cases in rules with alternation.
   So we need to support weighted random selection here. *)
let total_weight l =
    List.fold_left (fun x y -> x + y.weight) 0 l

let rec weighted_random acc r l =
    match l with
    | [] -> failwith "Cannot select anything from an empty list"
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

let rec reduce_rhs rhs grammar delimiter =
    match rhs with
    | [] -> ""
    | hd :: tl ->
        match hd with
        | Terminal hd -> Printf.sprintf "%s%s%s" hd delimiter (reduce_rhs tl grammar delimiter)
        | Nonterminal hd ->
            Printf.sprintf "%s%s" (reduce hd grammar delimiter) (reduce_rhs tl grammar delimiter)
and reduce name grammar delimiter =
    let r = find_production name grammar in
    match r with
    | None -> failwith (Printf.sprintf "Rule <%s> not found" name)
    | Some rhs ->
        let symbols = pick_element rhs in
        reduce_rhs symbols grammar delimiter
