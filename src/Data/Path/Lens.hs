{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
module Data.Path.Lens (Contextual(..), pathed, I(..)) where

import Data.Path.Class (IsPath)
import qualified Data.Path.Class as Path
import Control.Lens.Traversal (Traversal')
import Control.Lens.Internal (Context(..))
import Control.Lens.At (Index, IxValue, Ixed(..))

class Contextual (l :: * -> * -> *) where
  contextOf :: l a b -> Maybe (Context b b a)

pathed :: (IsPath p, Contextual l) => p l a b -> Traversal' a b
pathed p f a =
  case Path.uncons p of
    Path.UnconsEmpty ->
      f a
    Path.UnconsSome l p' ->
      case contextOf l of
        Nothing ->
          pure a
        Just (Context build val) ->
          build <$> pathed p' f val

newtype I (p :: (* -> * -> *) -> (* -> * -> *)) (l :: * -> * -> *) (a :: *) (b :: *) = I a
type role I nominal nominal representational nominal

type instance Index (I p l a b) = p l a b
type instance IxValue (I p l a b) = b
instance (IsPath p, Contextual l) => Ixed (I p l a b) where
  ix p f (I a) = I <$> pathed p f a