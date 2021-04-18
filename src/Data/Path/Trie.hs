{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

An efficient mapping from paths to values.
-}
module Data.Path.Trie (
  Trie,
  empty,
  singleton,
  insert,
  lookup,
  toDMap,
) where

import Data.Bifunctor.Flip (Flip (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Data.Path.Class (IsPath, Uncons (..))
import qualified Data.Path.Class as Path
import Prelude hiding (lookup)

data Trie l a f = Trie !(Maybe (f a)) !(DMap (l a) (Flip (Trie l) f))

empty :: Trie l a f
empty = Trie Nothing DMap.empty

singleton :: IsPath p => p l a b -> f b -> Trie l a f
singleton p val =
  case Path.uncons p of
    UnconsEmpty ->
      Trie (Just val) DMap.empty
    UnconsSome l p' ->
      Trie Nothing (DMap.singleton l (Flip (singleton p' val)))

insert :: (forall x. GCompare (l x)) => IsPath p => p l a b -> f b -> Trie l a f -> Trie l a f
insert p val (Trie mItem rest) =
  case Path.uncons p of
    UnconsEmpty ->
      Trie (Just val) rest
    UnconsSome l p' ->
      Trie
        mItem
        ( DMap.alter
            ( \mExisting ->
                Just . Flip $
                  case mExisting of
                    Nothing ->
                      singleton p' val
                    Just (Flip trie) ->
                      insert p' val trie
            )
            l
            rest
        )

lookup :: (forall x. GCompare (l x), IsPath p) => p l a b -> Trie l a f -> Maybe (f b)
lookup p (Trie mItem rest) =
  case Path.uncons p of
    UnconsEmpty ->
      mItem
    UnconsSome l p' -> do
      Flip trie <- DMap.lookup l rest
      lookup p' trie

toDMap :: forall p l a f. (GCompare (p l a), IsPath p) => Trie l a f -> DMap (p l a) f
toDMap = go Path.empty DMap.empty
 where
  go :: forall b. p l a b -> DMap (p l a) f -> Trie l b f -> DMap (p l a) f
  go p acc (Trie mItem rest) =
    DMap.foldlWithKey
      (\acc' l (Flip tries) -> go (Path.snoc p l) acc' tries)
      ( case mItem of
          Nothing ->
            acc
          Just item ->
            DMap.insert p item acc
      )
      rest