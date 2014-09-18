%{
    open Grammar
%}

%token <string> IDENTIFIER
%token <string> STRING
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

rule_rhs_part:
    | hd = symbol { [hd] }
    | tl = rule_rhs_part; hd = symbol { hd :: tl }
;

rule_rhs:
    | p = rule_rhs_part { [(List.rev p)] }
    | tl = rule_rhs; OR; hd = rule_rhs_part { (List.rev hd) :: tl }
;

rule:
     lhs = nonterminal; DEF; rhs = rule_rhs; { Rule (lhs, List.rev rhs) } 
;

rules:
    | (* empty *) { [] }
    | hd = rule { [hd] }
    | tl = rules; SEMI; hd = rule { hd :: tl } 
;

%public grammar:
    r = rules EOF { List.rev r }
;
