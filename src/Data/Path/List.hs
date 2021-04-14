{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Data.Path.List (Path (..)) where

import Control.Category (Category)
import qualified Control.Category (Category (..))
import Data.Path.Class (IsPath (..), Uncons (..))

data Path (l :: k -> k -> *) (a :: k) :: k -> * where
  Nil :: Path l a a
  Cons :: l a b -> Path l b c -> Path l a c

instance Category (Path l) where
  id = Nil
  (.) bc ab =
    case ab of
      Nil ->
        bc
      Cons ax xb ->
        Cons ax (bc Control.Category.. xb)

instance IsPath Path where
  cons = Cons
  uncons p =
    case p of
      Nil ->
        UnconsEmpty
      Cons l p' ->
        UnconsSome l p'
