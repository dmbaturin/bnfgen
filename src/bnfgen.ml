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

type cmd_action = Dump | Reduce

let print_version () = print_endline "2.0 (2019.06)"

let () = Random.self_init()

let filename = ref ""
let separator = ref " "
let start_symbol = ref "start"
let max_depth = ref None
let max_nonprod_depth = ref None
let action = ref Reduce
let debug = ref false
let args = [
    ("--dump-rules", Arg.Unit (fun () -> action := Dump), "Dump production rules and exit");
    ("--separator", Arg.String (fun s -> separator := s),
      "<string>  Token separator for generated output, default is space");
    ("--start", Arg.String (fun s -> start_symbol := s),
      "<string>  Start symbol, default is \"start\"");
    ("--debug", Arg.Unit (fun () -> debug := true), "Print debugging information to stderr");
    ("--max-reductions", Arg.Int (fun m -> max_depth := (Some m)), "<int> Maximum number of reductions to perform, default is infinite");
    ("--max-nonproductive-reductions", Arg.Int (fun m -> max_nonprod_depth := (Some m)), "<int> Maximum number of reductions that don't produce a terminal, default is infinite");
    ("--version", Arg.Unit (fun () -> print_version (); exit 0), "  Print version and exit")
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <BNF file>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun f -> filename := f) usage

let () =
    let g = Bnfgenlib.load_from_file !filename in
    match g with
    | Error msg -> Printf.eprintf "Could not load grammar from %s.\n%s%!\n" !filename msg; exit 1
    | Ok g ->
        begin match !action with
        | Dump -> Bnfgenlib.dump_rules g |> print_endline
        | Reduce ->
            let debug_fun = if !debug then (Printf.eprintf "%s\n%!") else ignore in
            let res = Bnfgenlib.generate ~debug:debug_fun ~max_depth:!max_depth ~max_non_productive:!max_nonprod_depth
              ~separator:!separator ~callback:print_string ~start_symbol:!start_symbol g
            in
            begin match res with
            | Ok _ -> print_string "\n"
            | Error msg -> Printf.eprintf "%s%!\n" msg
            end
        end
