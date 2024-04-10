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

let print_version () =
  print_endline "bnfgen 3.1.0";
  print_endline "Copyright 2024, Daniil Baturin et al., MIT license";
  print_endline "https://baturin.org/tools/bnfgen"

let () = Random.self_init()

let filename = ref ""
let start_symbol = ref "start"
let action = ref Reduce
let buffering = ref true
let productions = ref 1

let settings = ref {Bnfgen.default_settings with
  debug_fun=(Printf.eprintf "%s\n%!");
  symbol_separator = " "
}

let args = [
  ("--dump-rules", Arg.Unit (fun () -> action := Dump), "Dump grammar rules and exit");
  ("--separator", Arg.String (fun s -> settings := {!settings with symbol_separator=s}),
    "<string> Token separator for generated output, default is space");
  ("--start", Arg.String (fun s -> start_symbol := s),
    "<string>  Start symbol, default is \"start\"");
  ("--productions", Arg.Int ( fun p -> productions := p),
    "<int> Number of productions to output, a production is what is produced by the starting rule, default is 1");
  ("--debug", Arg.Unit (fun () -> settings := {!settings with debug=true}),
    "Print debugging information to stderr");
  ("--dump-stack", Arg.Unit (fun () -> settings := {!settings with debug=true; dump_stack=true}),
     "Include symbol stack in the debug output");
  ("--max-reductions", Arg.Int (fun m -> settings := {!settings with max_reductions=(Some m)}),
    "<int> Maximum number of reductions to perform, default is infinite");
  ("--max-nonproductive-reductions", Arg.Int (fun m -> settings := {!settings with max_nonproductive_reductions=(Some m)}),
    "<int> Maximum number of reductions that don't produce a terminal, default is infinite");
  ("--no-buffering", Arg.Unit (fun () -> buffering := false), "Disable output buffering");
  ("--version", Arg.Unit (fun () -> print_version (); exit 0), "Print version and exit")
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <BNF file>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun f -> filename := f) usage

let () =
  let g = Bnfgen.grammar_from_file !filename in
  match g with
  | Error msg -> Printf.eprintf "Could not load grammar from %s.\n%s%!\n" !filename msg; exit 1
  | Ok g ->
    begin match !action with
    | Dump -> Printf.printf "%s\n" @@ Bnfgen.grammar_to_string g
    | Reduce ->
      for production = 1 to !productions do
        if !settings.debug then Printf.ksprintf !settings.debug_fun "Outputting Production %d of %d%!" production !productions;
          let out_fun = if !buffering then print_string else (Printf.printf "%s%!") in
          let res = Bnfgen.generate ~settings:!settings out_fun g !start_symbol in
          begin match res with
          | Ok _ -> print_string "\n"
          | Error msg -> Printf.eprintf "%s%!\n" msg
          end
        done
      end
