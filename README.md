# automata-model
CSE 355 project to validate requirements through Finite Automata.

Eric Arellano, ecarell1

## Prerequisites
1. [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

## To install
Assumes Haskell Stack.

1. `stack setup`

## To run
1. `stack build`
1. `stack exec automata-model [path-to-input.txt]`

### REPL
1. `stack ghci`
1. `:set args [path-to-input.txt]`
1. `main`

##### Update project
`:r`

##### Quit REPL
`:q`


## Assumptions
1. The provided automata do not include an ID of 0. This is reserved to represent the empty set when converting NFAs to DFAs.
2. It is okay to renumber the states when converting from NFA -> DFA. This simplifies converting the powerset of converted State IDs (e.g. `[1, 2, 3]`) to a single number.
