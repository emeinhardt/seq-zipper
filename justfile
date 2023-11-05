default: cbuild

set ignore-comments := true

# NOTE all commands are executed as though from a shell at the project root,
# regardless of where you may be in a shell inside the project when you invoke
# a recipe.

alias c := cbuild
alias d := doc
alias n := nbuild

cbuild:
  cabal build

doc:
  ./dev/cabal-gen-docs.sh

c2n:
 cabal2nix . > ./pointed-word.nix

nbuild:
  nix build
