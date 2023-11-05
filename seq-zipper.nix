{ mkDerivation, base, base-unicode-symbols, comonad
, composition-prelude, containers, deepseq
, foldable1-classes-compat, lib, monoid-subclasses
, newtype-generics, nonempty-containers
}:
mkDerivation {
  pname = "seq-zipper";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base-unicode-symbols comonad composition-prelude containers
    deepseq foldable1-classes-compat monoid-subclasses newtype-generics
    nonempty-containers
  ];
  homepage = "https://github.com/emeinhardt/seq-zipper";
  description = "A non-empty comonadic Seq zipper";
  license = lib.licenses.mit;
}
