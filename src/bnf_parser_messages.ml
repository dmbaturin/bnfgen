
(* This file was auto-generated based on "src/bnf_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Err7\n"
    | 3 ->
        "Left-hand side of a rule must be a single non-terminal symbol.\nIn your rule, a symbol identifier is preceded by a string or the left-hand side is empty.\nThe most likely reason is a misplaced semicolon in a previous rule.\nExample of a valid rule: <start> ::= \"foo\" ;\n"
    | 5 ->
        "Left-hand side of a rule must be a single symbol identifier followed by \"::=\".\nIn your rule, a symbol identifier is followed by a string or the right-hand side is empty.\nExample of a valid rule: <foo> ::= \"foo\" ;\n"
    | 17 ->
        "Empty alternative in a right hand side.\nEmpty alternatives such as \"<foo> | | <bar>\") are not allowed.\nExample of a valid rule: <start> ::= <foo> | <bar> ;\n"
    | 14 ->
        "Weight number can only appear in the beginning of a rule alternative.\nIf you meant a literal number, write it in single or double quotes (\"42\" or '42').\nIf you want rule alternatives with different weight, insert a \"|\" between them.\nExample of a valid rule: <start> ::= 10 <foo> | 5 <bar> | \"baz\" ;\n"
    | 6 ->
        "The right-hand side of a rule is empty or starts with an empty alternative.\nEmpty rules (<foo> ::= ;) and empty alternatives (<foo> ::= | \"foo\") are not allowed.\nExample of a valid rule: <start> ::= <foo> | <bar> ;\n"
    | 11 ->
        "Weight number found not in the middle of a rule alternative.\nIf you meant a literal number, write it in single or double quotes (\"42\" or '42').\nIf you want rule alternatives with different weight, insert a \"|\" between them.\nExample of a valid rule: <start> ::= 10 <foo> | 5 <bar> | \"baz\" ;\n"
    | 8 ->
        "Weight number must be followed by a symbol identifier or a string.\nExample of a valid rule: <start> ::= 10 <foo> | <bar> ;\n"
    | _ ->
        raise Not_found
