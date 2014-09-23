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

%start <Grammar.rules> grammar
%%

/* Usual BNF with minor additions.
   Terminals are in double quotes. Nonterminals are in angle brackets.
   Left-hand and right-hand sides are separated by "::=".
   Rules are separated by double semicolon to handle multiline rules
   in LR(1)-friendly manner.

   As in:
     <start> ::= <nonterminal> "terminal" ;;
     <nonterminal> = "nonterminal"
 */

nonterminal:
    LANGLE; i = IDENTIFIER; RANGLE; { i }
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
    | n = NUMBER; r = rule_rhs_symbols { Rule_part (n, (List.rev r)) }
    | r = rule_rhs_symbols { Rule_part (1, (List.rev r)) }
;

rule_rhs:
    | p = rule_rhs_part { [p] }
    | tl = rule_rhs; OR; hd = rule_rhs_part { hd :: tl }
;

rule:
     lhs = nonterminal; DEF; rhs = rule_rhs; { Rule (lhs, Rule_rhs (sort_rule_parts rhs)) }
;

rules:
    | (* empty *) { [] }
    | hd = rule { [hd] }
    | tl = rules; SEMI; hd = rule { hd :: tl } 
;

%public grammar:
    r = rules EOF { List.rev r }
;
