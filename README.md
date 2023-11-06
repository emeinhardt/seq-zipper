# `seq-zipper`


This package defines a comonadic zipper for non-empty (finite) sequences, backed by [Data.Sequence](https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html) from the `containers` package.


## Comparison to similar `Zipper` types

Where a more familiar cons-list-backed non-empty comonadic list zipper principally allows for

 - Comonadic function application and composition.
 - Efficient linear traversal.
 - Efficient modification at or next to the focus value.

...a `Seq`-backed zipper *also* allows for

 - Efficient access to the outside edges of sequence.
 - Efficient random access to arbitrary locations in the prefix or suffix of the zipper.
 - Efficient movement of the cursor (focus) to arbitrary indices.

See the top-level documentation of `Data.Sequence.NonEmpty.Zipper` for more information.
