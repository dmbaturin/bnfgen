BNFGen
======

BNFGen generates random text based on context-free grammars.
You give it a file with your grammar, defined in BNF-like syntax,
it gives you a string that follows that grammar.

Can be used for parser fuzzing or amusement.

An online demo is available at https://baturin.org/tools/bnfgen

# Grammar syntax

The grammar syntax is an ad hoc extension of BNF (as if there's any other kind of BNF extensions).

Non-terminals are written in angle brackets (`<foo>`), terminals must be quoted (`'foo'` or `"foo"`).
It's ok to use the other kind of quotes inside quoted strings without escaping, that is, `"foo 'bar' baz"`
or `'foo "bar" baz'` are fine. Same kind of quote must be escaped with a backslash.

To enable writing multi-line rules easily, rules must be separated with a semicolon.

Empty alternatives are not allowed in rules with alternation, i.e. `<foo> ::= <bar> | <baz>;` is fine, while
`<foo> ::= | <bar> | <baz>;` is not.

Here is a simple example:

```
$ cat examples/02_english.bnf 
# A trivial subset of English.
<start> ::= <NP> <VP> ;
<NP> ::= <article> <subject> ;
<VP> ::= <verb> <article> <object> ;
<article> ::= "a" | "the" ;
<subject> ::= "customer"
            | "system administrator"
            | "programmer" ;
<object> ::= "system" | "compiler" | "program" ;
<verb> ::= "used" | "installed" | "developed"

$ bnfgen examples/02_english.bnf
a system administrator installed a compiler
```

The default start symbol is `<start>`, but you can override it with `--start` option.
For example, `bnfgen --start NP examples/02_english.bnf` will only produce noun phrases.

By default BNFGen uses space as a token separator, but you can specify any separator
with `--separator` option, including empty string (`--separator ""`).

## Repetition

There is no direct support for repetition, but it can be easily done with
recursive rules.

Here are recipes for common repetition patterns:

```
# <foo>?
<one_or_more_foo> ::= <foo> | "" ;

# <foo>*
<zero_or_more_foo> ::= <foo> <zero_or_more_foo> | "" ;

# <foo>+
<one_or_more_foo> ::= <foo> <one_or_more_foo> | <foo> ;

```

BNFGen makes no attempt to check if a production will ever terminate,
and even terminating rules may product a lot of data.
You can limit recursion depth with `--max-depth` option.
Note that `--max-depth 0` means "no recursion" rather than infinite.
To allow infinite recursion, simply omit that option.

## Weighted random

A problem with using simple BNF for producing test cases for parser fuzzing
is that if each alternative is taken equally often, there's a 50% chance
that production will terminate immediately with the non-recursive case.
Likewise, you simply may want some symbols more frequent than others to give your
text a more realistic appearance.

The "killer feature" of BNFGen is that it gives you control over recursion depth.
You can specify a "weight" for every alternative to choose how often they should be taken.
The default weight is 1.

For example, if you want to produce long strings of "a", you can use this rule
to make BNFGen choose the recursive alternative hundred times more often.

```
<start> ::= 100 <start> "a" | "a" ;
```

You can give different weight to every alternative. For example, the following
grammar will product strings with lots of a's, some b'c, and few c's.

```
<foo> ::= 100 "a" | 50 "b" | "c" ;

<start> ::= 100 <foo> <start> | <foo>
```

# Installation

From the OPAM repository: `opam install bnfgen`.

From a local repo clone: `opam install -w .`.

```
opam pin add bnfgen .
```

You can also find some binaries in the GitHub releases.

# Usage

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
