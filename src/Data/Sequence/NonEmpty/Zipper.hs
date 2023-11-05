{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- -Wno-incomplete-patterns is here because of del, del', and https://github.com/haskell/containers/issues/590

{- |

This module defines a comonadic zipper for non-empty (finite) sequences, backed by "Data.Sequence".

== Comparison to similar 'Zipper' types

Where a more familiar cons-list-backed non-empty list zipper comonad principally allows for

 - Comonadic function application and composition.
 - Efficient linear traversal.
 - Efficient modification at or next to the focus value.

...a 'Seq'-backed zipper /also/ allows for

 - Efficient access to the outside edges of sequence.
 - Efficient random access to arbitrary locations in the prefix or suffix of the zipper.
 - Efficient movement of the cursor ("focus") to arbitrary indices.


__Note__ that where a normal list zipper has a prefix whose head is the element of the prefix
/closest to the focus/, the current 'Zipper' implementation here maintains normal linear order of the prefix.
Determining the trade-offs of this decision and refactoring in light of those conclusions is a priority
for future refactoring. For normal comonadic non-empty list zippers the order of the prefix is important to its
key complexity trade-offs. I haven't thought yet about what the derivative of the underlying `FingerTree` type
is and what that might suggest about what a "natural" comonadic zipper over a `FingerTree` (and hence a `Seq`)
should be

In the meantime I have erred in the direction of pragmatism for my motivating use case; comments and pull
requests are welcome on this — or any other aspect of the module.


=== Motivation

The motivating context for this module is supporting research in formal language and automata theory —
representing structures like pointed words and contextual transformations on pointed words. This is why
there are more methods and instances than might otherwise be expected for a variation on a comonadic list
zipper.

For the same reasons, there is an @IsString (Zipper a)@ instance for any @TextualMonoid a@ and an
@IsList (Zipper a)@ instance for any @Monoid a@:

>>> import Data.Sequence.NonEmpty.Zipper
>>> :set -XOverloadedLists
>>> :set -XOverloadedStrings
>>> ["foo", "bar"] ∷ Zipper String
Zipper (fromList []) "foo" (fromList ["bar"])
>>> [] ∷ Zipper String
Zipper (fromList []) "" (fromList [])
>>> "abc" ∷ Zipper String
Zipper (fromList []) "a" (fromList ["b","c"])
>>> "" ∷ Zipper String
Zipper (fromList []) "" (fromList [])


== Importing the module

Like related packages that overlap with the "Data.List" interface, you will need to import this module
qualified or use @NoImplicitPrelude@ to access some of this module's API, or to access the optimized versions
over the generic functionality provided by related typeclasses: this affects

 - 'reverse'
 - 'length'
 - 'elem'
 - 'toList'
-}

-- TODO write seq-zipper-test package, focusing on verifying 'monoid-subclasses' instances

-- TODO Calculate the derivative for 2-3 finger trees and figure out what that suggests about what the zipper
-- for Data.Sequence would be — maybe it's closely related, maybe it looks very different.
-- TODO more Semigroupoid instances / other things downstream of Foldable1 should be added as needed.
-- TODO Traversable instance for SuffixCat

module Data.Sequence.NonEmpty.Zipper
  ( -- * Construction
    Zipper (..)
  , unZipper
  , zipper
  , Data.Sequence.NonEmpty.Zipper.singleton
  , fromNESeqL
  , fromNESeqR
  , fromNonEmptyL
  , fromNonEmptyR
  , fromSeqL
  , fromSeqR
  , fromListL
  , fromListR
  , toNESeq
  , toNonEmpty
  , Data.Sequence.NonEmpty.Zipper.toSeq
  , Data.Sequence.NonEmpty.Zipper.toList

    -- * Accessors and queries
  , circumfix
  , length
  , index
  , index'
  , isBeg
  , isEnd
  , noPrefix
  , noSuffix
  , noCircumfix
  , Data.Sequence.NonEmpty.Zipper.elem

    -- ** Find
    -- | Searches through a zipper are specified by location and direction of movement relative to the focus.
  , Region (Prefix, Suffix, Circumfix, Focus, PrefixFocus, SuffixFocus, CircumfixFocus)
  , Dir (I, O)
  , findIndices
  , findIndex

    -- * Traversal
  , next
  , prev
  , beg
  , end
  , shiftL
  , shiftR
  , shift
  , jump
  , jump'

    -- * Updates
  , sub
  , del
  , del'
  , cmap
  , pmap
  , smap
  , zmap
  , zmap'
  , insR
  , delR
  , insL
  , delL
  , reverse
  , squashFocus
  , squashedFocus
  , squashSeq
  , squash

    -- * Newtypes
    -- |
    -- The 'Semigroup' instance for 'Zipper' preserves the focus of the right argument:
    --
    --  - The result of @l <> r@ is a 'Zipper' formed by concatenating @l@ to the prefix of @r@, and hence
    --    the focus of @l <> r@ is the focus of @r@.
    --
    -- This makes @Zipper a@ a 'LeftReductive' semigroup and 'LeftGCDMonoid' (when 'a' is a @Monoid@), but
    --  not a 'RightReductive' semigroup and not a 'RightGCDMonoid'.
    --
    -- The 'SuffixCat' newtype has the opposite semigroup instance, preserves the focus of its left argument,
    -- and is a 'RightReductive' semigroup, 'RightGCDMonoid' (when 'a' is a @Monoid@), but is not a
    -- 'LeftReductive' semigroup and not a 'LeftGCDMonoid'.
  , SuffixCat (SuffixCat, unSuffixCat)

    -- * Not-quite store comonad interface
    -- |
    -- Like many other common containers, for every @a@, @Zipper a@ /almost/ defines a store comonad instance
    -- with index 'Int'; because not every 'Int' (not even every @Nat@) is a valid index that a 'Zipper' can
    -- produce a result for, you can't write a definition of @peek ∷ Int → Zipper → a@ for the instance.
    --
    -- Note, however, that a variant that had a statically known size (a representable functor) would yield
    -- such an instance, however, as would a variant of a @Zipper@ that was constrained (e.g. via GADT
    -- constructor constraints) to carry 'Monoid' values. In both cases, @peek@ would be total.
    --
    -- The partial interface below comes close to the store comonad interface, however.
  , pos'
  , peek'
  , peeks'
  , seeks'
  , experiment'
  ) where

import Prelude hiding
  ( length
  , reverse
  )

import Prelude.Unicode
  ( (∘)
  )
import Data.Function
  ( on
  )
import Control.Composition
  ( (.*)
  )
import Control.Arrow
  ( (&&&)
  )

import GHC.Generics
  ( Generic
  , Generic1
  )
import Control.Newtype.Generics
  ( Newtype
  )
import Control.DeepSeq
  ( NFData
  )

import Data.Maybe
  ( fromJust
  )

import Data.Foldable          qualified as F
import Data.Foldable1
  ( Foldable1 ( foldMap1
              )
  )
import Data.List              qualified as L
import Data.List.NonEmpty     qualified as NE
import Data.List.NonEmpty
  ( NonEmpty ( (:|)
             )
  )
import Data.Sequence          qualified as S
import Data.Sequence.NonEmpty qualified as NES
import Data.Sequence
  ( Seq ( Empty
        , (:<|)
        , (:|>)
        )
  )
import Data.Sequence.NonEmpty
  ( NESeq ( (:<||)
          )
  )

import Control.Monad
  ( join
  )
import Control.Comonad
  ( Comonad ( extract
            , duplicate
            )
  )

import Data.Semigroup.Factorial qualified as SF
import Data.Semigroup.Factorial
  ( Factorial (factors)
  , StableFactorial
  )
import Data.Semigroup.Cancellative
  ( LeftReductive  (isPrefixOf, stripPrefix)
  , RightReductive (isSuffixOf, stripSuffix)
  , LeftCancellative
  , RightCancellative
  )
import Data.Monoid.GCD
  ( LeftGCDMonoid  (commonPrefix)
  , RightGCDMonoid (commonSuffix)
  )
import Data.Monoid.Null qualified as MN
import Data.Monoid.Null
  ( MonoidNull
  , PositiveMonoid
  )
-- import Data.Monoid.Factorial -- TODO
--   ( FactorialMonoid, splitPrimePrefix
--   )

import Data.Monoid.Textual
  ( TextualMonoid
  )
import Data.String
  ( IsString (fromString)
  )
import GHC.IsList
  ( IsList ( fromList
           , toList
           , Item
           )
  )





---------------
-- CONSTRUCTION
---------------

data Zipper a = Zipper { prefix ∷ Seq a, focus ∷ a, suffix ∷ Seq a }
  deriving stock (Eq, Ord, Show, Read, Generic, Generic1)

instance NFData a ⇒ NFData (Zipper a)

-- | Destructure a 'Zipper' into its components.
unZipper ∷ ∀ a. Zipper a → (Seq a, a, Seq a)
unZipper (Zipper p x s) = (p, x, s)
{-# INLINABLE unZipper #-}

-- | Case analysis for a 'Zipper'.
zipper ∷ ∀ a b. (a → b) → (a → Seq a → b) → (Seq a → a → b) → (Seq a → a → Seq a → b) → Zipper a → b
zipper f _ _ _ (Zipper Empty x Empty) = f   x
zipper _ f _ _ (Zipper Empty x s    ) = f   x s
zipper _ _ f _ (Zipper p     x Empty) = f p x
zipper _ _ _ f (Zipper p     x s    ) = f p x s
{-# INLINABLE zipper #-}

-- | Create a 'Zipper' containing a single value.
singleton ∷ ∀ a. a → Zipper a
singleton = flip (Zipper Empty) Empty
{-# INLINABLE singleton #-}

-- | Construct a 'Zipper' from an 'NESeq' with the focus at the leftmost value.
fromNESeqL ∷ ∀ a. NESeq a → Zipper a
fromNESeqL = uncurry (Zipper Empty) ∘ (NES.head &&& NES.tail)
{-# INLINABLE fromNESeqL #-}

-- | Construct a 'Zipper' from an 'NESeq' with the focus at the rightmost value.
fromNESeqR ∷ ∀ a. NESeq a → Zipper a
fromNESeqR as = Zipper (NES.init as) (NES.last as) Empty
{-# INLINABLE fromNESeqR #-}

-- | Construct a 'Zipper' from a 'NonEmpty' with the focus at the leftmost value.
fromNonEmptyL ∷ ∀ a. NonEmpty a → Zipper a
fromNonEmptyL as = Zipper Empty (NE.head as) (S.fromList $ NE.tail as)
{-# INLINABLE fromNonEmptyL #-}

-- | Construct a 'Zipper' from a 'NonEmpty' with the focus at the rightmost value.
fromNonEmptyR ∷ ∀ a. NonEmpty a → Zipper a
fromNonEmptyR as = Zipper (S.fromList $ NE.init as) (NE.last as) Empty
{-# INLINABLE fromNonEmptyR #-}

-- | Construct a 'Zipper' from a 'Seq' with the focus at the leftmost value.
fromSeqL ∷ ∀ a. Seq a → Maybe (Zipper a)
fromSeqL Empty      = Nothing
fromSeqL (a :<| as) = Just $ Zipper Empty a as
{-# INLINABLE fromSeqL #-}

-- | Construct a 'Zipper' from an 'Seq' with the focus at the rightmost value.
fromSeqR ∷ ∀ a. Seq a → Maybe (Zipper a)
fromSeqR Empty      = Nothing
fromSeqR (as :|> a) = Just $ Zipper as a Empty
{-# INLINABLE fromSeqR #-}

-- | Construct a 'Zipper' from a cons list with the focus at the leftmost value.
fromListL ∷ ∀ a. [a] → Maybe (Zipper a)
fromListL []     = Nothing
fromListL (a:as) = Just $ Zipper Empty a (S.fromList as)
{-# INLINABLE fromListL #-}

-- | Construct a 'Zipper' from a cons list with the focus at the rightmost value.
fromListR ∷ ∀ a. [a] → Maybe (Zipper a)
fromListR [] = Nothing
fromListR as = Just $ Zipper (S.fromList $ L.init as) (L.head as) Empty
{-# INLINABLE fromListR #-}

-- | Construct an 'NESeq' from a 'Zipper'.
toNESeq ∷ ∀ a. Zipper a → NESeq a
toNESeq (Zipper p x s) = p NES.><| NES.singleton x NES.|>< s
{-# INLINABLE toNESeq #-}

-- | Construct a 'Seq' from a 'Zipper'.
toSeq ∷ ∀ a. Zipper a → Seq a
toSeq (Zipper p x s) = (p S.|> x) S.>< s
{-# INLINABLE toSeq #-}

-- | Construct a 'NonEmpty' from a 'Zipper'.
toNonEmpty ∷ ∀ a. Zipper a → NonEmpty a
toNonEmpty z = let nes = toNESeq z in NES.head nes :| (F.toList ∘ NES.tail $ nes)
{-# INLINABLE toNonEmpty #-}

-- | Construct a cons list from a 'Zipper'.
toList ∷ ∀ a. Zipper a → [a]
toList (Zipper Empty x Empty) = [x]
toList (Zipper p     x Empty) = F.toList $ p :|> x
toList (Zipper Empty x s    ) = F.toList $ x :<| s
toList (Zipper p     x s    ) = F.toList   ((p :|> x) S.>< s)
{-# INLINABLE toList #-}



------------------------
-- QUERIES AND ACCESSORS
------------------------

-- | The prefix and the suffix around the focus.
circumfix ∷ ∀ a. Zipper a → (Seq a, Seq a)
circumfix (Zipper p _ s) = (p, s)
{-# INLINABLE circumfix #-}

length ∷ ∀ a. Zipper a → Int
length (Zipper p _ s) = 1 + S.length p + S.length s
{-# INLINABLE length #-}

-- | The index of the current value (from the left).
index ∷ ∀ a. Zipper a → Int
index = S.length ∘ prefix
{-# INLINABLE index #-}

-- | The distance of the current value from the right.
index' ∷ ∀ a. Zipper a → Int
index' = S.length ∘ suffix
{-# INLINABLE index' #-}

-- | Convert a distance from the right edge of the zipper to an index (distance from the left edge).
distFromEndToIndex ∷ ∀ a. Zipper a → Int → Maybe Int
distFromEndToIndex z i
  | i < 0 || i > length z = Nothing
  | otherwise             = Just $ length z - 1 - i
{-# INLINABLE distFromEndToIndex #-}

-- | Is the zipper's cursor at the left edge of the sequence?
isBeg ∷ ∀ a. Zipper a → Bool
isBeg = S.null ∘ prefix
{-# INLINABLE isBeg #-}

-- | Is the zipper's cursor at the right edge of the sequence?
isEnd ∷ ∀ a. Zipper a → Bool
isEnd = S.null ∘ suffix
{-# INLINABLE isEnd #-}

-- | Is the zipper's prefix null? Synonym for 'isBeg'.
noPrefix ∷ ∀ a. Zipper a → Bool
noPrefix (Zipper Empty _ _) = True
noPrefix _                  = False
{-# INLINABLE noPrefix #-}

-- | Is the zipper's suffix null? Synonym for 'isEnd'.
noSuffix ∷ ∀ a. Zipper a → Bool
noSuffix (Zipper _ _ Empty) = True
noSuffix _                  = False
{-# INLINABLE noSuffix #-}

-- | Are both the prefix and suffix null? (Is the zipper a singleton?)
noCircumfix ∷ ∀ a. Zipper a → Bool
noCircumfix (Zipper Empty _ Empty) = True
noCircumfix _                      = False
{-# INLINABLE noCircumfix #-}

-- | Is the value an element of the zipper?
elem ∷ ∀ a. (Eq a) ⇒ a → Zipper a → Bool
elem a (Zipper p x s) = (a == x) || F.elem a p || F.elem a s
{-# INLINABLE elem #-}


{- | An enum describing all possible regions of the zipper; @PrefixFocus@, @SuffixFocus@, etc. indicate
the region including both the @Prefix@, etc. /and/ the focus. -}
data Region = Prefix | Suffix | Circumfix | Focus | PrefixFocus | SuffixFocus | CircumfixFocus
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

{- | An enum describing direction of travel of a traversal relative to the focus:

  - Inwards towards the focus.
  - Outwards away from the focus.
-}
data Dir = I | O
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

{- | Find all indices in the specified region of the zipper that satisfy the predicate, traversing
the region with the specified direction of motion.

(If you want searches whose direction is not defined relative to the focus, you should move the focus to
one end of the zipper or the other, or convert the zipper to some other structure.)

 - For outwards circumfix searches, results are sorted in ascending order by distance from the focus.
 - For inwards  circumfix searches, results are sorted in ascending order by distance from the nearest edge.

In both cases, for pairs of values with the same distance, values from the prefix come before values from
the suffix. -}
findIndices ∷ ∀ a. Region → Dir → (a → Bool) → Zipper a → [Int]
findIndices Focus          _ p z =          findF  p z
findIndices Prefix         I p z =          findPR p z
findIndices Prefix         O p z =          findPL p z
findIndices Suffix         I p z =          findSL p z
findIndices Suffix         O p z =          findSR p z
findIndices PrefixFocus    I p z =          findPR p z  ++    findF  p z
findIndices PrefixFocus    O p z =          findF  p z  ++    findPL p z
findIndices SuffixFocus    I p z =          findSL p z  ++    findF  p z
findIndices SuffixFocus    O p z =          findF  p z  ++    findSR p z
findIndices Circumfix      I p z = e z $ λ (findPR p z) ++ ρ (findSL p z)
findIndices Circumfix      O p z = c z $ λ (findPL p z) ++ ρ (findSR p z)
findIndices CircumfixFocus I p z = e z $ λ (findPR p z) ++ λ (findF  p z) ++ ρ (findSL p z)
findIndices CircumfixFocus O p z = c z $ λ (findF  p z) ++ λ (findPL p z) ++ ρ (findSR p z)
{-# INLINABLE findIndices #-}

λ ∷ [a] → [Either a a]
λ = fmap Left

ρ ∷ [a] → [Either a a]
ρ = fmap Right

unEither ∷ Either a a → a
unEither (Left  a) = a
unEither (Right a) = a

{- | Map a list of tagged indices (Lefts are from the prefix, Rights from the suffix) of a given zipper
to a list of indices sorted by ascending distance from the focus of the zipper, where indices with the same
distance are broken by sorting prefix indices over suffix indices. -}
c ∷ Zipper a → [Either Int Int] → [Int]
c z = fmap unEither ∘ L.sortOn (TaggedDist ∘ fmap abs ∘ fmap (fromJust ∘ indexToOffset' z))

{- | Map a list of tagged indices (Lefts are from the prefix, Rights from the suffix) of a given zipper
to a list of indices sorted by ascending distance from the edges of the zipper (== descending distance from
the focus), where indices with the same distance are broken by sorting prefix indices over suffix indices. -}
e ∷ Zipper a → [Either Int Int] → [Int]
e z = fmap unEither ∘ L.sortOn (TaggedDist ∘ fmap abs ∘ fmap (e' z))

-- | Map an index inside the zipper to its distance from the nearest outside edge.
e' ∷ Zipper a → Int → Int
e' z i = if i <= index z then i else index' z

newtype TaggedDist = TaggedDist { unTaggedDist ∷ Either Int Int }
  deriving stock (Eq, Show, Generic)

instance Ord TaggedDist where
  (TaggedDist (Left  l)) <= (TaggedDist (Left  r)) = l <= r
  (TaggedDist (Right l)) <= (TaggedDist (Right r)) = l <= r
  (TaggedDist (Left  l)) <= (TaggedDist (Right r))
    | l == r    = True
    | otherwise = l <= r
  (TaggedDist (Right r)) <= (TaggedDist (Left l))
    | r == l    = False
    | otherwise = r <= l

{- | Return the first index (if any) of the element satisfying the predicate that is in the specified
region(s) and encountered while traveling in the specified direction relative to the focus. -}
findIndex ∷ ∀ a. Region → Dir → (a → Bool) → Zipper a → Maybe Int
findIndex r d p = let f []    = Nothing
                      f (i:_) = Just i
                  in  (f ∘ take 1) ∘ findIndices r d p
{-# INLINABLE findIndex #-}

-- | If the focus element satisfies the predicate, return its index as a singleton; otherwise return [].
findF ∷ ∀ a. (a → Bool) → Zipper a → [Int]
findF p z@(Zipper _ x _)
  | p x       = [index z]
  | otherwise = []

{- | Find the indices of all elements in the prefix that satisfy the predicate,
searching L→R. -}
findPL ∷ ∀ a. (a → Bool) → Zipper a → [Int]
findPL p (Zipper ps _ _) = S.findIndicesL p ps

{- | Find the indices of all elements in the prefix that satisfy the predicate,
searching R→L. -}
findPR ∷ ∀ a. (a → Bool) → Zipper a → [Int]
findPR p (Zipper ps _ _) = S.findIndicesR p ps

{- | Find the indices of all elements in the suffix that satisfy the predicate,
searching R→L. -}
findSR ∷ ∀ a. (a → Bool) → Zipper a → [Int]
findSR p z@(Zipper _ _ ss) = (+ index z) ∘ (+ 1) <$> S.findIndicesR p ss

{- | Find the indices of all elements in the suffix that satisfy the predicate,
searching L→R. -}
findSL ∷ ∀ a. (a → Bool) → Zipper a → [Int]
findSL p z@(Zipper _ _ ss) = (+ index z) ∘ (+ 1) <$> S.findIndicesL p ss



-------------
-- TRAVERSALS
-------------

-- | Shift the focus to the left one position, if possible.
prev ∷ ∀ a. Zipper a → Maybe (Zipper a)
prev (Zipper Empty      _ _ ) = Nothing
prev (Zipper (ps :|> p) x ss) = Just $ Zipper ps p (x :<| ss)
{-# INLINABLE prev #-}

-- | Shift the focus to the right one position, if possible.
next ∷ ∀ a. Zipper a → Maybe (Zipper a)
next (Zipper _  _ Empty     ) = Nothing
next (Zipper ps x (s :<| ss)) = Just $ Zipper (ps :|> x) s ss
{-# INLINABLE next #-}

-- | Shift the focus to the leftmost value.
beg ∷ ∀ a. Zipper a → Zipper a
beg z@(Zipper Empty      _ _) = z
beg   (Zipper (p :<| ps) x s) = Zipper Empty p ((ps :|> x) S.>< s)
{-# INLINABLE beg   #-}

-- | Shift the focus to the rightmost value.
end ∷ ∀ a. Zipper a → Zipper a
end z@(Zipper _  _ Empty     ) = z
end   (Zipper ps x (ss :|> s)) = Zipper ((ps :|> x) S.>< ss) s Empty
{-# INLINABLE end   #-}

-- {- | Convert an "absolute" index (distance from the left edge) to an offset from the focus.

-- For example,

--  - @indexToOffset z 3 = (-1)@ means that the focus is at index 4: the offset from 4 to 3 is -1.
--  - @indexToOffset z 5 =   2 @ means that the focus is at index 3: the offset from 3 to 5 is  2.

-- @indexToOffset z i@ is undefined when @i < 0@. Otherwise it is total. This means that even if a zipper @z@
-- has length @n@, then @indexToOffset z (n+k)@ for @k >= 0@ is still defined, even though no such position
-- actually exists in @z@. -}
-- indexToOffset ∷ ∀ a. Zipper a → Int → Maybe Int
-- indexToOffset z i
--   | i < 0     = Nothing
--   | otherwise = Just $ -1 * (index z - i)

-- -- | Inverse of 'indexToOffset'.
-- offsetToIndex ∷ ∀ a. Zipper a → Int → Int
-- offsetToIndex z 0 = index z
-- offsetToIndex z i = index z + i

-- -- | Inverse of 'indexToOffset''. Returns @Nothing@ iff the result would be an invalid position for the zipper.
-- offsetToIndex' ∷ ∀ a. Zipper a → Int → Maybe Int
-- offsetToIndex' z 0 = Just $ index z
-- offsetToIndex' z i =
--   let i' = index z + i
--   in  if (i' < 0 || i' > (length z - 1)) then Nothing else Just i'

{- | Like 'indexToOffset', but only returns a @Just@ value when the @Int@ argument is a valid index of the
zipper. -}
indexToOffset' ∷ ∀ a. Zipper a → Int → Maybe Int
indexToOffset' z i
  | i < 0              = Nothing
  | i > (length z - 1) = Nothing
  | otherwise          = Just $ -1 * (index z - i)
{-# INLINABLE indexToOffset' #-}

-- | Shift the focus to the left @i@ positions, if possible. Undefined for @i < 0@ and @i - 1 > length p@.
shiftL ∷ ∀ a. Zipper a → Int → Maybe (Zipper a)
shiftL z                0 = Just z
shiftL (Zipper p x s) i
  | (i - 1) < 0 || (i - 1) >= S.length p = Nothing
  | otherwise               = shiftLto $ indexRtoIndexL (S.length p) (i - 1) where
      indexRtoIndexL l k = l - (k + 1)
      shiftLto j =
        let (preJ, postJ') = S.splitAt j p
            xOf Empty      = x
            xOf (x_ :<| _) = x_
            postJ          = S.drop 1 postJ'
            p'             = preJ
            x'             = xOf postJ'
            s'             = postJ S.>< (x :<| s)
        in  Just $ Zipper p' x' s'
{-# INLINABLE shiftL #-}

-- | Shift the focus to the right @i@ positions, if possible. Undefined for @i < 0@ and @i - 1 > length s@.
shiftR ∷ ∀ a. Zipper a → Int → Maybe (Zipper a)
shiftR z                0                = Just z
shiftR (Zipper p x s) i
  | (i - 1) < 0 || (i - 1) >= S.length s = Nothing
  | otherwise               = shiftRto (i - 1) where
      shiftRto j =
        let (preJ, postJ') = S.splitAt j s
            xOf Empty      = x
            xOf (x_ :<| _) = x_
            postJ          = S.drop 1 postJ'
            p'             = (p :|> x) S.>< preJ
            x'             = xOf postJ'
            s'             = postJ
        in  Just $ Zipper p' x' s'
{-# INLINABLE shiftR #-}

-- | Shift the focus by @i@, if possible. Negative shifts move leftward, positive shifts move rightward.
shift ∷ ∀ a. Zipper a → Int → Maybe (Zipper a)
shift z 0     = Just z
shift z i
  | i < 0     = shiftL z (-1 * i)
  | otherwise = shiftR z i
{-# INLINABLE shift #-}

-- | Update the focus to the specified index (distance from the /left/ edge), if that index exists.
jump ∷ ∀ a. Zipper a → Int → Maybe (Zipper a)
jump z i = shift z =<< indexToOffset' z i
{-# INLINABLE jump #-}

{- | Update the focus to the index with the specified distance from the /right/ edge,
if such an index exists. -}
jump' ∷ ∀ a. Zipper a → Int → Maybe (Zipper a)
jump' z i = jump z =<< distFromEndToIndex z i
{-# INLINABLE jump' #-}



------------------
-- (OTHER) UPDATES
------------------

-- | Substitute the focus value with something else.
sub ∷ ∀ a. a → Zipper a → Zipper a
sub y (Zipper p _ s) = Zipper p y s
{-# INLINABLE sub #-}

-- | Delete the focus value and shift left by default; fails iff the circumfix is empty.
del ∷ ∀ a. Zipper a → Maybe (Zipper a)
del (Zipper Empty      _ Empty) = Nothing
del (Zipper (ps :|> p) _ ss   ) = Just $ Zipper ps p ss
-- incomplete pattern match warning here is irrelevant: see https://github.com/haskell/containers/issues/590
{-# INLINABLE del #-}

-- | Delete the focus value and shift right by default; fails iff the circumfix is empty.
del' ∷ ∀ a. Zipper a → Maybe (Zipper a)
del' (Zipper Empty _ Empty     ) = Nothing
del' (Zipper ps    _ (s :<| ss)) = Just $ Zipper ps s ss
-- incomplete pattern match warning here is irrelevant: see https://github.com/haskell/containers/issues/590
{-# INLINABLE del' #-}

-- | Update the focus ("centered") value via a function that has access only to the focus and nothing else.
cmap ∷ ∀ a. (a → a) → Zipper a → Zipper a
cmap f (Zipper p x s) = Zipper p (f x) s
{-# INLINABLE cmap #-}

-- | Update the focus value via a function that has access to the prefix and the focus and nothing else.
pmap ∷ ∀ a. (Seq a → a → a) → Zipper a → Zipper a
pmap f (Zipper p x s) = Zipper p (f p x) s
{-# INLINABLE pmap #-}

-- | Update the focus value via a function that has access to the focus and the suffix and nothing else.
smap ∷ ∀ a. (a → Seq a → a) → Zipper a → Zipper a
smap f (Zipper p x s) = Zipper p (f x s) s
{-# INLINABLE smap #-}

{- | Update the focus value via a function that has access to the prefix, the focus, and the suffix.

Like comonadic 'extend', except that it only updates the focal value and cannot change the type of the focus.
-}
zmap ∷ ∀ a. (Seq a → a → Seq a → a) → Zipper a → Zipper a
zmap f (Zipper p x s) = Zipper p (f p x s) s
{-# INLINABLE zmap #-}

-- | Minor variant of 'zmap' that may compose more concisely.
zmap' ∷ ∀ a. (Zipper a → a) → Zipper a → Zipper a
zmap' f z@(Zipper p _ s) = Zipper p (f z) s
{-# INLINABLE zmap' #-}


-- | Insert a value to the right of the focus.
insR ∷ ∀ a. a → Zipper a → Zipper a
insR y (Zipper p x s) = Zipper p x (y :<| s)
{-# INLINABLE insR #-}

-- | Insert a value to the left of the focus.
insL ∷ ∀ a. a → Zipper a → Zipper a
insL y (Zipper p x s) = Zipper (p :|> y) x s
{-# INLINABLE insL #-}

-- | Delete the value to the left of the focus if one exists and fail otherwise.
delL ∷ ∀ a. Zipper a → Maybe (Zipper a)
delL (Zipper Empty      _ _ ) = Nothing
delL (Zipper (ps :|> _) x ss) = Just $ Zipper ps x ss
{-# INLINABLE delL #-}

-- | Delete the value to the right of the focus if one exists and fail otherwise.
delR ∷ ∀ a. Zipper a → Maybe (Zipper a)
delR (Zipper _  _ Empty     ) = Nothing
delR (Zipper ps x (_ :<| ss)) = Just $ Zipper ps x ss
{-# INLINABLE delR #-}

-- | Reverse the total ordering of elements in the zipper.
reverse ∷ ∀ a. Zipper a → Zipper a
reverse (Zipper ls x rs) = Zipper (S.reverse rs) x (S.reverse ls)
{-# INLINABLE reverse #-}


{- | Squash a @Seq s@ of elements with a @Factorial@ instance so that the result is a @Seq@
of @s@ values such that each @s@ value in the @Seq@ has a list of prime factors of length 1. -}
squashSeq ∷ ∀ s. (Factorial s) ⇒ Seq s → Seq s
squashSeq ss
  | F.all (== 1) $ (L.length ∘ SF.factors) <$> ss = ss
  | otherwise                                   = foldMap (S.fromList ∘ SF.factors) ss
{-# INLINABLE squashSeq #-}

{- | For a type @s@ with a @Factorial@ instance, split the focus @x@ of a 'Zipper' @z@ into a 'Zipper' @z'@
consisting of the values

 - Before @midIndex x@ — the prefix of @z'@.
 - A singleton @midIndex x@ — the focus of @z'@.
 - After @midIndex x@ — the suffix of @z'@.

The result will be a 'Zipper' of elements in @x@ whose factors are all singletons with respect to @s@;
note that this means the prefix and suffix of @z@ will not be present at all in @z'@. -}
squashedFocus ∷ ∀ s. (Factorial s) ⇒ Zipper s → Zipper s
squashedFocus (Zipper _ x _) =
  let x'        = S.fromList ∘ SF.factors $ x -- a flat (Seq s) of prime factors of x
      mi        = midIndex x'
      (xp, xs') = S.splitAt mi x'
      xm        = fromJust $ x' S.!? mi
      xs        = S.drop 1 xs'
  in Zipper xp xm xs
{-# INLINABLE squashedFocus #-}


{- | Given a zipper over a 'Factorial' semigroup, @squashFocus z@ "squashes" the focus value by
splitting the focal value into

  - A middle factor.
  - Every factor to the left  of the middle factor within the focus.
  - Every factor to the right of the middle factor within the focus.

and then

 - Successively suffixing each focus factor left  of the middle of the focus to the prefix of @z@.
 - Successively prefixing each focus factor right of the middle of the focus to the suffix of @z@.

The result is a zipper whose total ordering over factors is the same, but with a focal value
whose list of factors is a singleton.

Note that for a value @z@ of length @l@, the index @i@ of the middle factor is calculated as
@floor (l / 2)@.

The main intended use case for this and related functions is working with bikleisli arrows from a 'Zipper'
comonad value into a monad @m@ (probably 'Seq'); @squashFocus@ facilitates normalizing the output 'Zipper'
of a 'Zipper'-to-@m@ bikleisli arrow to a 'Zipper' where /every/ position has a factors list that is a
singleton.

The term "squash" is used here rather than "flatten", because "flatten" is typically used to mean
collapsing two or more levels of nesting some type @m@ within itself (e.g. a list of lists), which is
similar but not what this operation is. -}
squashFocus ∷ ∀ a. (Factorial a) ⇒ Zipper a → Zipper a
squashFocus z@(Zipper Empty _ Empty) = squashedFocus z
squashFocus z@(Zipper Empty _ s    ) = let (Zipper px xx sx) = squashFocus z in Zipper px xx (sx <> s)
squashFocus z@(Zipper p     _ Empty) = let (Zipper px xx sx) = squashFocus z in Zipper (p <> px) xx sx
squashFocus z@(Zipper p     _ s    ) = let (Zipper px xx sx) = squashFocus z in Zipper (p <> px) xx (sx <> s)
{-# INLINABLE squashFocus #-}

-- | Squash the focus, squash the prefix, and squash the suffix.
squash ∷ ∀ a. (Factorial a) ⇒ Zipper a → Zipper a
squash z@(Zipper p _ s) =
  let (Zipper px xx sx) = squashFocus z
  in  Zipper (squashSeq p <> px) xx (sx <> squashSeq s)
{-# INLINABLE squash #-}

-- | Return the index @m@ of the middle factor of some @Factorial@ @s@, @m = floor $ (SF.length s) / 2@.
midIndex ∷ ∀ s. (Factorial s) ⇒ s → Int
midIndex s = let l ∷ Int
                 l  = SF.length s
                 m ∷ Double → Int
                 m  = floor ∘ (/ 2)
             in  m ∘ fromIntegral @Int @Double $ l




instance Foldable Zipper where
  foldMap f (Zipper ls x rs) = foldMap f ls <> f x <> foldMap f rs

  toList = Data.Sequence.NonEmpty.Zipper.toList

  length = Data.Sequence.NonEmpty.Zipper.length

  elem = Data.Sequence.NonEmpty.Zipper.elem

instance Traversable Zipper where
  traverse f (Zipper p x s) = Zipper <$> traverse f p <*> f x <*> traverse f s

{- | The package defining nonempty sequences doesn't look like it's been updated at all since 2021 and
this library is a package with fairly niche expected appeal, so I see little risk in defining an orphan
instance here. If/when it is updated, this can be wrapped in a newtype. Pull requests for making this
safer are also welcome. -}
instance Foldable1 NESeq where
  foldMap1 f (x :<|| xs) = go (f x) xs where
    go y Empty      = y
    go y (z :<| zs) = y <> go (f z) zs

instance Foldable1 Zipper where
  foldMap1 f (Zipper Empty      x Empty     ) = f x
  foldMap1 f (Zipper Empty      x (s :<| ss)) = f x <> foldMap1 f (s :<|| ss)
  foldMap1 f (Zipper (p :<| ps) x Empty     ) = foldMap1 f (p :<|| ps) <> f x
  foldMap1 f (Zipper (p :<| ps) x (s :<| ss)) = foldMap1 f (p :<|| ps) <> f x <> foldMap1 f (s :<|| ss)

-- TODO more Semigroupoid instances / other things downstream of Foldable1 should be added as needed...



{- | @l <> r@ concatenates @l@ into the prefix of @r@: @focus $ (l <> r) == focus r@. This makes
'Zipper' 'LeftReductive' but not 'RightReductive'. -}
instance Semigroup (Zipper a) where
  (<>) ∷ Zipper a → Zipper a → Zipper a
  l <> (Zipper p x s) = Zipper (Data.Sequence.NonEmpty.Zipper.toSeq l <> p) x s

instance Factorial (Zipper a) where
  factors ∷ Zipper a → [Zipper a]
  factors z = F.toList $ fmap Data.Sequence.NonEmpty.Zipper.singleton z

instance StableFactorial (Zipper a)

{- | @(SuffixCat l) <> (SuffixCat r)@ concatenates @r@ into the suffix of @l@:
@focus $ (SuffixCat l) <> (SuffixCat r) == focus l@. This makes @SuffixCat@ 'RightReductive' but not
'LeftReductive'. -}
newtype SuffixCat a = SuffixCat { unSuffixCat ∷ Zipper a }
  deriving stock   (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (Foldable, Foldable1)
  deriving newtype (Functor, Comonad)
  deriving newtype (Factorial, StableFactorial)
  deriving newtype (Monoid, MonoidNull, PositiveMonoid)
  deriving newtype (IsString)
  deriving anyclass Newtype

-- instance Traversable SuffixCat where  -- TODO


-- | @l <> r@ concatenates @r@ to the suffix of @l@: the focus of @l <> r@ is the focus of @l@.
instance Semigroup (SuffixCat a) where
  (SuffixCat (Zipper p x s)) <> (SuffixCat r) =
    SuffixCat $ Zipper p x (s <> Data.Sequence.NonEmpty.Zipper.toSeq r)


{- | @\`isPrefixOf\ w`@ reflects (depends) on the focus of @w@:
@p \`isPrefixOf\` w@ iff @(toSeq p) \`isPrefixOf\` (prefix w)@. -}
instance (Eq a) ⇒ LeftReductive (Zipper a) where
  -- isPrefixOf ∷ Eq a ⇒ Zipper a → Zipper a → Bool
   p `isPrefixOf` w =
     Data.Sequence.NonEmpty.Zipper.toSeq p `isPrefixOf` prefix w

   -- stripPrefix ∷ Eq a → Zipper a → Zipper a → Maybe (Zipper a)
   p `stripPrefix` (Zipper pw xw sw) =
     let p' = Data.Sequence.NonEmpty.Zipper.toSeq p
     in  (\p_ → Zipper p_ xw sw) <$> (p' `stripPrefix` pw)

instance (Eq a) ⇒ LeftCancellative (Zipper a)

instance (Eq a, Monoid a) ⇒ LeftGCDMonoid (Zipper a) where
  commonPrefix = fromJust ∘ fromSeqL .* (commonPrefix `on` prefix)


{- | @\`isSuffixOf\ w`@ reflects (depends) on the focus of @w@:
@s \`isSuffixOf\` w@ iff @(toSeq ∘ unSuffixCat $ s) \`isSuffixOf\` (suffix ∘ unsuffixCat $  w)@. -}
instance (Eq a) ⇒ RightReductive (SuffixCat a) where
   (SuffixCat s) `isSuffixOf` (SuffixCat w) =
     Data.Sequence.NonEmpty.Zipper.toSeq s `isSuffixOf` suffix w

   (SuffixCat s) `stripSuffix` (SuffixCat (Zipper pw xw sw)) =
     let s' = Data.Sequence.NonEmpty.Zipper.toSeq s
     in  (SuffixCat ∘ Zipper pw xw) <$> (s' `stripSuffix` sw)

instance (Eq a) ⇒ RightCancellative (SuffixCat a)

instance (Eq a, Monoid a) ⇒ RightGCDMonoid (SuffixCat a) where
  commonSuffix = SuffixCat ∘ fromJust ∘ fromSeqL .* (commonSuffix `on` suffix ∘ unSuffixCat)



instance (Monoid a) ⇒ Monoid (Zipper a) where
  mempty = Zipper Empty mempty Empty

instance (MonoidNull a) ⇒ MonoidNull (Zipper a) where
  -- null ∷ Zipper a → Bool
  null (Zipper Empty x Empty) = MN.null x
  null _                      = False

instance (MonoidNull a) ⇒ PositiveMonoid (Zipper a)

-- instance (MonoidNull a) ⇒ FactorialMonoid (Zipper a) where  -- TODO + SuffixCat
--   -- splitPrimePrefix ∷ Zipper a ⇒ Maybe (Zipper a, Zipper a)
--   splitPrimePrefix z
--     | MN.null z = Nothing
--     | otherwise = undefined



instance (TextualMonoid a) ⇒ IsString (Zipper a) where
  fromString s
    | MN.null s = mempty
    | otherwise = fromJust ∘ fromSeqL ∘ S.fromList ∘ SF.factors ∘ fromString $ s

instance (Monoid a) ⇒ IsList (Zipper a) where
  type Item (Zipper a) = a

  fromList l
    | L.null l  = mempty
    | otherwise = fromJust ∘ Data.Sequence.NonEmpty.Zipper.fromListL $ l

  toList = Data.Sequence.NonEmpty.Zipper.toList

instance (Monoid a) ⇒ IsList (SuffixCat a) where
  type Item (SuffixCat a) = a

  fromList l
    | L.null l  = mempty
    | otherwise = SuffixCat ∘ fromJust ∘ Data.Sequence.NonEmpty.Zipper.fromListL $ l

  toList = Data.Sequence.NonEmpty.Zipper.toList ∘ unSuffixCat




instance Functor Zipper where
  fmap f (Zipper p x s) = Zipper (f <$> p) (f x) (f <$> s)

instance Comonad Zipper where
  extract ∷ Zipper a → a
  extract = focus

  duplicate ∷ Zipper a → Zipper (Zipper a)
  duplicate z = Zipper p z s
    -- Note the asymmetry of the unfolds here:
    --  - If the prefix field had the first element be the one closest to the focus,
    --    both p and s where would be unfoldr
    where p = S.unfoldl (fmap (join (,)) ∘ prev) z
          s = S.unfoldr (fmap (join (,)) ∘ next) z

-- | Synonym for 'index'.
pos' ∷ ∀ a. Zipper a → Int
pos' = index
{-# INLINABLE pos' #-}

-- | Peek at the value (if any) at an index.
peek' ∷ ∀ a. Int → Zipper a → Maybe a
peek' = fmap focus .* flip jump
{-# INLINABLE peek' #-}

-- | Transform the current index and peek at the value (if any) there.
peeks' ∷ ∀ a. (Int → Int) → Zipper a → Maybe a
peeks' f = peek' =<< (f ∘ index)
{-# INLINABLE peeks' #-}

-- | Flipped synonym for 'jump'.
seeks' ∷ ∀ a. Int → Zipper a → Maybe (Zipper a)
seeks' = flip jump
{-# INLINABLE seeks' #-}

{- | Variant of 'peeks'': Transform the current index into a container of indices, and extract the values
(if any) at each of those indices. -}
experiment' ∷ ∀ f a. Functor f ⇒ (Int → f Int) → Zipper a → f (Maybe a)
experiment' f = fmap ∘ flip peek' <*> f ∘ pos'
{-# INLINABLE experiment' #-}
