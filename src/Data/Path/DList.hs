{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Path.DList (Path (..), fromList, toList) where

import Control.Category (Category)
import qualified Control.Category (Category (..))
import Data.Path.Class (IsPath (..), SplitAt (..), Uncons (..), Unsnoc (..), append)
import qualified Data.Path.List as List
import Prelude hiding (splitAt)

newtype Path (l :: k -> k -> *) (a :: k) (b :: k) = Path (forall x. List.Path l b x -> List.Path l a x)

instance Category (Path l) where
  id = Path id
  (.) (Path bc) (Path ab) = Path (ab . bc)

instance IsPath Path where
  cons lab (Path bc) = Path (List.Cons lab . bc)
  uncons ab =
    case uncons (toList ab) of
      UnconsEmpty ->
        UnconsEmpty
      UnconsSome lax xb ->
        UnconsSome lax (Path (xb `append`))

  snoc (Path ab) lbc = Path (ab . List.Cons lbc)
  unsnoc ab =
    case unsnoc (toList ab) of
      UnsnocEmpty ->
        UnsnocEmpty
      UnsnocSome ax lxb ->
        UnsnocSome (Path (ax `append`)) lxb

  composeR f ab zbc =
    composeR f (toList ab) zbc

  composeL f zab bc =
    composeL f zab (toList bc)

  splitAt n ab =
    case splitAt n (toList ab) of
      SplitAt ax xb ->
        SplitAt (fromList ax) (fromList xb)

fromList :: List.Path l a b -> Path l a b
fromList ab = Path (ab `append`)

toList :: Path l a b -> List.Path l a b
toList (Path ab) = ab List.Nil