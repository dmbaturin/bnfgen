(*
 * Copyright (c) 2014, 2021 Daniil Baturin
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

%{
    open Grammar
%}

%token <string> IDENTIFIER
%token <string> STRING
%token <int> NUMBER
%token DEF
%token OR
%token EOF
%token SEMI
%token LBRACE
%token RBRACE
%token COMMA

%start <Grammar.grammar> grammar
%%

/* Usual BNF with minor additions.
   Terminals are in double quotes. Nonterminals are in angle brackets.
   Left-hand and right-hand sides are separated by "::=".
   Rules are separated by a semicolon to handle multiline rules
   in an LR(1)-friendly manner.
   Rule parts may contain "weight" before symbols,
   that affects how often they are selected.

   As in:
     <start> ::= 10 <nonterminal> "terminal" | "terminal" ;
     <nonterminal> = "nonterminal"
 */

nonterminal:
    x = IDENTIFIER { x }
;

terminal:
    s = STRING { s }
;

repeat_range:
    | LBRACE; n = NUMBER; RBRACE { (n, n) }
    | LBRACE; l = NUMBER; COMMA; r = NUMBER; RBRACE { (l, r) }
;

symbol:
    | s = terminal { Terminal s }
    | s = nonterminal { Nonterminal s }
    | s = symbol; r = repeat_range { Repeat (s, r) }
;

rule_rhs_symbols:
    | hd = symbol { [hd] }
    | tl = rule_rhs_symbols; hd = symbol { hd :: tl }
;

rule_rhs_part:
    | n = NUMBER; r = rule_rhs_symbols {
          { weight = n; symbols = (List.rev r) }
      }
    | r = rule_rhs_symbols {
          { weight = 1; symbols = (List.rev r) }
      }
;

rule_rhs:
    | p = rule_rhs_part { [p] }
    | tl = rule_rhs; OR; hd = rule_rhs_part { hd :: tl }
;

rule:
     r = separated_pair(nonterminal, DEF, rule_rhs) {
         { lhs = (fst r); rhs = (sort_rule_parts (snd r)) }
     }
;

rules:
    | (* empty *) { [] }
    | hd = rule { [hd] }
    | tl = rules; SEMI; hd = rule; { hd :: tl }
;

%public grammar:
    r = rules; option(SEMI); EOF { List.rev r }
;
