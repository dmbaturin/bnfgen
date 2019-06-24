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

{

open Bnf_parser

exception Error of string

let lexing_error lexbuf msg =
  let line, column = Util.get_lexing_position lexbuf in
  let err = Printf.sprintf "Syntax error on line %d, character %d: %s" line column msg in
  raise (Error err)

}

rule token = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t' '\r' ]
    { token lexbuf }
| "::="
    { DEF }
| '|'
    { OR }
| '<'
    { LANGLE }
| '>'
    { RANGLE }
| ";"
    { SEMI }
| [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' '_' ]+ as s
    { IDENTIFIER s}
| ['0' - '9']+ as i
    { NUMBER (int_of_string i) }
| eof
    { EOF }
| '''
    { read_single_quoted_string (Buffer.create 16) lexbuf }
| '"'
    { read_double_quoted_string (Buffer.create 16) lexbuf }
| '#' [^ '\n']+ '\n'
    { Lexing.new_line lexbuf ; token lexbuf }
| _ as bad_char
{ lexing_error lexbuf (Printf.sprintf "unexpected character \'%c\'" bad_char) }

and read_double_quoted_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_double_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_double_quoted_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_double_quoted_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_double_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing double quote" }

and read_single_quoted_string buf =
  parse
  | '''       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_single_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_single_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_single_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_single_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_single_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_single_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_single_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_single_quoted_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_single_quoted_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_single_quoted_string buf lexbuf }
  | [^ ''' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing single quote" }
