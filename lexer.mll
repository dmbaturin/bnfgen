{

exception Error of string

type token = 
    | IDENTIFIER of string
    | STRING of string
    | DEF
    | OR
    | LANGLE
    | RANGLE
    | EOF
    | SEMI

}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| "::="
    { DEF }
| '|'
    { OR }
| '<'
    { LANGLE }
| '>'
    { RANGLE }
| ";;"
    { SEMI }
| ['a' - 'z' 'A' - 'Z']+ as s
    { IDENTIFIER s}
| eof
    { EOF }
| '"'
    { read_string (Buffer.create 17) lexbuf }
| _
{ raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("String is not terminated")) }
