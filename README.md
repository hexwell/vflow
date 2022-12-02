# vflow - variable flow

Static code analyzer checking for variable dependencies between modules (variable flow).

# Language Syntax

The term vflow is used to refer to both the tool and the language. <br>
Snippets of vflow can be inserted in files written in other languages. The examples will be in Bash. <br>


At the top level, vflow snippets contain blocks. <br>
A block can be either an Imports or an Exports block.

## Imports Blocks
Imports blocks start with an Imports directive. <br>
Imports block contain variables. <br>
These are the required variables that must be satisfied by other modules. <br>
Variables are listed one for each line, indented one level more than the Imports directive. <br>


Either directives can be used to specify a list of variables, of which at least and at most one must be satisfied.
Either directives can be used inside an Imports block, indented one level more than the Imports directive. <br>
Variables are listed one for each line, indented one level more than the Either directive. <br>


Optionals directives can be used to specify a list of optional variables. <br>
Optionals directives can be used inside an Imports block, indented at the same level as the Imports directive. <br>
Variables are listed one for each line, indented one level more than the Optionals directive. <br>

Comments for variables can be specified by adding a colon after the variable name and typing the comment after the colon. <br>

This is an example of an Imports block:
```bash

# IMPORTS:
#   a
#   b : bbbb
#   c
#
#   Either:
#     d
#     f
#
# Optionals:
#   g
#   h

```

This declares required variables a, b (with comment bbbb), c and either d or f, and declares optional variables g and h.


# Exports Blocks

Exports blocks start with an Exports directive. <br>
Exports block contain variables. <br>
These variables can satisfy requirements in other modules. <br>
Variables are listed one for each line, indented one level more than the Exports directive. <br>

An override modifier can be added before the variable name to indicate that the variable overrides a previously defined variable with the same name. <br>

Comments for variables can be specified by adding a colon after the variable name and typing the comment after the colon. <br>

This is an example of an Exports block:
```bash

# EXPORTS:
#   a : aaaa
#   b
#   override c : cccc

```

This declares variables a (with comment aaaa) and b (without comment) and overrides variable c (with comment cccc).

# TODO

Use [language-bash](https://hackage.haskell.org/package/language-bash) to actually parse bash for variables (and names/tokens/identifiers in general) uses (and generate vflow snippets?)?
