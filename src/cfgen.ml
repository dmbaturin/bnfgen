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

open Util
open Grammar

type cmd_action = Dump | Reduce

let load_grammar filename =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Parse_bnf.parse lexbuf (Bnf_parser.Incremental.grammar lexbuf.lex_curr_p)

let () = Random.self_init()

let () =
    let filename = ref "" in
    let separator = ref " " in
    let start_symbol = ref "start" in
    let max_depth = ref None in
    let action = ref Reduce in
    let args = [
        ("--dump-rules", Arg.Unit (fun () -> action := Dump),
         "Dump production rules and exit");
        ("--separator", Arg.String (fun s -> separator := s),
         "<string>  Token separator for generated output, default is space");
        ("--start", Arg.String (fun s -> start_symbol := s),
        "<string>  Start symbol, default is \"start\"");
        ("--max-depth", Arg.Int (fun m -> max_depth := (Some m)), "<int>  Maximum recursion depth, default is infinite")
    ] in let usage = Printf.sprintf "Usage: %s [OPTIONS] <BNF file>" Sys.argv.(0) in
    if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
    else
        begin
            Arg.parse args (fun f -> filename := f) usage;
            try
                let g = load_grammar !filename in
                let () = check_for_duplicates g in
                match !action with
                | Dump -> string_of_rules g |> print_endline
                | Reduce -> reduce ~maxdepth:!max_depth !start_symbol g !separator |> print_endline
            with
            | Syntax_error (pos, err) ->
                begin
                    match pos with
                    | Some (line, pos) -> Printf.eprintf "Syntax error on line %d, character %d: %s\n%!" line pos err
                    | None -> Printf.eprintf "Syntax error: %s\n%!" err
                end
            | Failure msg ->
                Printf.eprintf "Error: %s\n%!" msg;
                exit 1
        end
