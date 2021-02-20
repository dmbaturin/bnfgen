BNFGen
======

BNFGen generates random text based on context-free grammars.
You give it a file with your grammar, defined using BNF-like syntax,
it gives you a string that follows that grammar.

BNFGen is:

* a CLI tool
* an OCaml library

There are also official JS bindings available via [NPM](https://www.npmjs.com/package/bnfgen).

Project goals:

* Make it easy to write and share grammars.
* Give the user total control of and insight into the generation process.

An online demo is available at https://baturin.org/tools/bnfgen

So, how does BNFGen achieve those goals?

## Grammar syntax

BNFGen provides a "DSL" for grammar definitions. It's a familiar BNF-like syntax with a few additions.

* Terminals are in single or double quotes (`"foo"`, `'bar'`).
* Non-terminals are in angle brackets: `<baz>`, `<quux>`.
* Rules are separated by semicolons.

```
# My first BNFGen grammar
<start> ::= <greeting> "world" ;

<greeting> ::= "hello" | "high"
```

If you get the syntax wrong, you'll (usually) get a helpful syntax error message.

```
$ cat bad.bnf 
<start> ::= ;

$ bnfgen bad.bnf 
Could not load grammar from bad.bnf.
Syntax error on line 1, character 13: The right-hand side of a rule is empty or starts with an empty alternative.
Empty rules (<foo> ::= ;) and empty alternatives (<foo> ::= | "foo") are not allowed.
Example of a valid rule: <start> ::= <foo> | <bar> ;
```

One problem with using straight BNF for driving language _generators_ is that you have no control
over the process. BNFGen adds two features to fix that.

### Weighted random

You can specify a "weight" for a rule alternative. For example, this rule will make BNFGen take the `"hello"`
alternative ten times more often.

```
<greeting> ::= 10 "hello" | "hi" ;
```

The canonical way to express repetition in BNF is to use a self-referential recursive rule. In classic BNF,
that can easily lead to the process terminating to early, since there's a 50% chance that it will
take the non-recursive alternative.

BNFGen allows you to influence the chances and make the recursive alternative more likely to produce longer sentences.

```
<start> ::= 10 <start> "foo" | "foo" ;
```

### Deterministic repetition

Finally, for a completely predictable result, you can use repetition ranges.

Exactly ten of `foo`: `<start> ::= "foo"{10}`.

Up to ten of `foo`: `<start> ::= "foo"{1,10}`.


# Installation

From the OPAM repository: `opam install bnfgen`.

From a local repo clone: `opam install -w .`.

You can also find some binaries in the GitHub releases.

# CLI tool usage

```
Usage: bnfgen [OPTIONS] <BNF file>
  --dump-rules  Dump production rules and exit
  --separator <string>  Token separator for generated output, default is space
  --start <string>  Start symbol, default is "start"
  --max-reductions <int>  Maximum reductions, default is infinite
  --max-nonproductive-reductions <int>  Maximum number of reductions that don't produce a terminal, default is infinite
  --debug  Enable debug output (symbols processed, alternatives taken...)
  --dump-stack  Show symbol stack for every reduction (implies --debug)
  --version   Print version and exit
  -help   Display this list of options
  --help  Display this list of options

```

Running `bnfgen --debug --dump-stack` will make it log every reduction step and show you the current symbol stack,
so that you know what it's doing and can see where your grammar is looping or growing out of control.

# Library usage example

```ocaml
# let g = Bnfgen.grammar_from_string " <greeting> ::= \"hello\" | \"hi\" ; <start> ::= <greeting> \"world\"; " |> Result.get_ok ;;
val g : Bnfgen.Grammar.grammar =
  [("greeting",
    [{Bnfgen.Grammar.weight = 1; symbols = [Bnfgen.Grammar.Terminal "hi"]};
     {Bnfgen.Grammar.weight = 1; symbols = [Bnfgen.Grammar.Terminal "hello"]}]);
   ("start",
    [{Bnfgen.Grammar.weight = 1;
      symbols =
       [Bnfgen.Grammar.Nonterminal "greeting"; Bnfgen.Grammar.Terminal "world"]}])]

# Bnfgen.generate_string ~settings:({Bnfgen.default_settings with symbol_separator=" "}) g "start" ;;
- : (string, string) result = Ok "hello world "

# Bnfgen.generate ~settings:({Bnfgen.default_settings with symbol_separator=""}) print_endline g "start" ;;
hello world
- : (unit, string) result = Ok ()
```
