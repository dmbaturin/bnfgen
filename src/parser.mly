%{
    open Grammar
%}

%token <string> IDENTIFIER
%token <string> STRING
%token <int> NUMBER
%token DEF
%token OR
%token LANGLE
%token RANGLE
%token EOF
%token SEMI

%start <Grammar.grammar> grammar
%%

/* Usual BNF with minor additions.
   Terminals are in double quotes. Nonterminals are in angle brackets.
   Left-hand and right-hand sides are separated by "::=".
   Rules are separated by double semicolon to handle multiline rules
   in LR(1)-friendly manner.
   Rule parts may contain "weight" before symbols,
   that affects how often they are selected.

   As in:
     <start> ::= 10 <nonterminal> "terminal" | "terminal" ;;
     <nonterminal> = "nonterminal"
 */

nonterminal:
    x = delimited(LANGLE, IDENTIFIER, RANGLE) { x }
;

terminal:
    s = STRING { s }
;

symbol:
    | s = terminal { Terminal s }
    | s = nonterminal { Nonterminal s }
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
