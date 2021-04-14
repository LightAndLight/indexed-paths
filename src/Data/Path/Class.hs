{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Data.Path.Class (
  Uncons (..),
  Unsnoc (..),
  SplitAt (..),
  IsPath (..),
  empty,
  append,
) where

import Control.Category (Category)
import qualified Control.Category as Category
import Prelude hiding (splitAt)

data Uncons (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  UnconsSome :: l a b -> p l b c -> Uncons p l a c
  UnconsEmpty :: Uncons p l a a

data Unsnoc (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  UnsnocSome :: p l a b -> l b c -> Unsnoc p l a c
  UnsnocEmpty :: Unsnoc p l a a

data SplitAt (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  SplitAt :: p l a b -> p l b c -> SplitAt p l a c

{-|

Laws:

```
uncons (cons a b) = UnconsSome a b

unsnoc (snoc a b) = UnsnocSome a b

cons a empty = singleton = snoc empty a

composeR cons p empty = p

composeL snoc empty p = p

forget . singleton = id
```

-}
class (forall l. Category (p l)) => IsPath (p :: (k -> k -> *) -> k -> k -> *) where
  cons :: l a b -> p l b c -> p l a c
  uncons :: p l a b -> Uncons p l a b

  composeR :: (forall x y z. l x y -> r y z -> r x z) -> p l a b -> r b c -> r a c
  composeR f p z =
    case uncons p of
      UnconsEmpty ->
        z
      UnconsSome lab bc ->
        f lab (composeR f bc z)

  composeL :: (forall x y z. r x y -> l y z -> r x z) -> r a b -> p l b c -> r a c
  composeL f z p =
    case uncons p of
      UnconsEmpty ->
        z
      UnconsSome lab bc ->
        composeL f (f z lab) bc

  forget :: Category l => p l a b -> l a b
  forget = composeL (Category.>>>) Category.id

  singleton :: l a b -> p l a b
  singleton l = cons l empty

  snoc :: p l a b -> l b c -> p l a c
  snoc p l = append p (singleton l)

  unsnoc :: p l a b -> Unsnoc p l a b
  unsnoc p =
    case uncons p of
      UnconsEmpty ->
        UnsnocEmpty
      UnconsSome k p' ->
        case unsnoc p' of
          UnsnocEmpty ->
            UnsnocSome empty k
          UnsnocSome p'' k' ->
            UnsnocSome (cons k p'') k'

  splitAt :: Int -> p l a b -> SplitAt p l a b
  splitAt !n p =
    if n <= 0
      then SplitAt p empty
      else case uncons p of
        UnconsEmpty ->
          SplitAt empty empty
        UnconsSome l p' ->
          case splitAt (n - 1) p' of
            SplitAt pfx sfx ->
              SplitAt (cons l pfx) sfx

empty :: IsPath p => p l a a
empty = Category.id

append :: IsPath p => p l a b -> p l b c -> p l a c
append = flip (Category..)