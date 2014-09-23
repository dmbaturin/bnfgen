cfgen
=====

Generates string based on context-free grammars

## Building

You need the OCaml compiler and menhir.

```
ocamlbuild -use-menhir -menhir "menhir -v --external-tokens Lexer" main.native
```

## Usage

```
cfgen [OPTIONS] <grammar file>
```

Available options:

| Option               | Meaning                                      |
| -------------------- | ---------------------------------------------|
| --start <string>     | Change start symbol, default is "start"      |
| --separator <string> | Change token separator, default is space     |
| --help               | Print help and exit                          |

Grammar is specified in slightly modified BNF:
* Nonterminals are in angle brackets, <foo>
* Terminals are in double quotes, "foo"
* Left and right hand sides of a rule are separated by "::="
* Rules are separated by ";;"
* You can specify "weight" for rule options

A grammar that produces strings that consist of "aaa" word can look like:

```
<start> ::= 10 <start> <aaa> | <aaa> ;;

<aaa> ::=  "aaa" ;;
```

The weight 10 means the recursive part will be chosen 10 times more often than
the other part, to avoid terminating too early.
