cfgen
=====

Generates string based on context-free grammars.
Can be used for parser fuzzing or amusement.

```
$ cat examples/02_english.bnf 
# A trivial subset of English.
<start> ::= <NP> <VP> ;;
<NP> ::= <article> <subject> ;;
<VP> ::= <verb> <article> <object> ;;
<article> ::= "a" | "the" ;;
<subject> ::= "customer"
            | "system administrator"
            | "programmer" ;;
<object> ::= "system" | "compiler" | "program" ;;

$ cfgen examples/02_english.bnf
a system administrator installed a compiler
```

## Building

You need the OCaml compiler, menhir, ocamlbuild, and oasis.

```
ocaml setup.ml -configure [--prefix <prefix>]
ocaml setup.ml -build
ocaml setup.ml -install

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
