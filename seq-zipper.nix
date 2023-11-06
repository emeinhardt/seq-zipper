{ mkDerivation, base, base-unicode-symbols, comonad
, composition-prelude, containers, deepseq, lib, monoid-subclasses
, newtype-generics, nonempty-containers, semigroupoids
}:
mkDerivation {
  pname = "seq-zipper";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base-unicode-symbols comonad composition-prelude containers
    deepseq monoid-subclasses newtype-generics nonempty-containers
    semigroupoids
  ];
  homepage = "https://github.com/emeinhardt/seq-zipper";
  description = "A non-empty comonadic Seq zipper";
  license = lib.licenses.mit;
}
