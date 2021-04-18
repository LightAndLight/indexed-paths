{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Path.Class (
  Uncons (..),
  Unsnoc (..),
  SplitAt (..),
  IsPath (..),
  empty,
  append,
  compose,
) where

import Control.Category (Category)
import qualified Control.Category as Category
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Show (GShow (..))
import Data.Type.Equality ((:~:) (..))
import Prelude hiding (splitAt)

data Uncons (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  UnconsSome :: l a b -> p l b c -> Uncons p l a c
  UnconsEmpty :: Uncons p l a a

data Unsnoc (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  UnsnocSome :: p l a b -> l b c -> Unsnoc p l a c
  UnsnocEmpty :: Unsnoc p l a a

data SplitAt (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) (a :: k) :: k -> * where
  SplitAt :: p l a b -> p l b c -> SplitAt p l a c

{- |

Laws:

```
uncons (cons a b) = UnconsSome a b

unsnoc (snoc a b) = UnsnocSome a b

cons a empty = singleton = snoc empty a

composeR cons p empty = p

composeL snoc empty p = p

splitAt n xs = SplitAt a b ==> append a b = xs
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

  composeMap :: Category c => (forall x y. l x y -> c x y) -> p l a b -> c a b
  composeMap f = composeL (\acc l -> acc Category.>>> f l) Category.id

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

{- |

```
compose . singleton = id
```
-}
compose :: (IsPath p, Category l) => p l a b -> l a b
compose = composeMap id

newtype APath (p :: (k -> k -> *) -> k -> k -> *) (l :: k -> k -> *) a b = APath (p l a b)

instance (IsPath p, forall x y. Show (l x y)) => Show (APath p l a b) where
  show (APath p) =
    "["
      <> ( case uncons p of
            UnconsEmpty ->
              mempty
            UnconsSome h t ->
              go h t id ""
         )
      <> "]"
   where
    go :: l x y -> p l y z -> ShowS -> ShowS
    go h t acc =
      let acc' = acc . (show h <>)
       in case uncons t of
            UnconsEmpty ->
              acc'
            UnconsSome h' t' ->
              go h' t' (acc' . (", " <>))

instance (IsPath p, forall x y. Show (l x y)) => GShow (APath p l a) where
  gshowsPrec = showsPrec

instance (IsPath p, forall x. GEq (l x)) => GEq (APath p l a) where
  geq :: APath p l a x -> APath p l a x' -> Maybe (x :~: x')
  geq (APath p1) (APath p2) =
    case uncons p1 of
      UnconsEmpty ->
        case uncons p2 of
          UnconsEmpty ->
            Just Refl
          UnconsSome{} ->
            Nothing
      UnconsSome h1 t1 ->
        case uncons p2 of
          UnconsEmpty ->
            Nothing
          UnconsSome h2 t2 -> do
            Refl <- geq h1 h2
            geq (APath t1) (APath t2)

instance (IsPath p, forall x. GCompare (l x)) => GCompare (APath p l a) where
  gcompare :: APath p l a x -> APath p l a x' -> GOrdering x x'
  gcompare (APath p1) (APath p2) =
    case uncons p1 of
      UnconsEmpty ->
        case uncons p2 of
          UnconsEmpty ->
            GEQ
          UnconsSome{} ->
            GLT
      UnconsSome h1 t1 ->
        case uncons p2 of
          UnconsEmpty ->
            GGT
          UnconsSome h2 t2 -> do
            case gcompare h1 h2 of
              GLT -> GLT
              GGT -> GGT
              GEQ -> gcompare (APath t1) (APath t2)