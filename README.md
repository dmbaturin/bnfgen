cfgen
=====

Generates string based on context-free grammars

## Building

You need the OCaml compiler and menhir.

```
ocamlbuild -use-menhir -menhir "menhir -v --external-tokens Lexer" main.native
```
