{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Path.Lens (ToTraversal (..), pathed, T (..), I (..)) where

import Control.Category (Category)
import qualified Control.Category (Category (..))
import Control.Lens.At (Index, IxValue, Ixed (..))
import Control.Lens.Traversal (Traversal')
import Data.Path.Class (IsPath (composeMap))

class ToTraversal (l :: * -> * -> *) where
  toTraversal :: l a b -> Traversal' a b

newtype T a b = T {unT :: Traversal' a b}

instance Category T where
  id = T id
  (.) (T a) (T b) = T (b . a)

pathed :: (IsPath p, ToTraversal l) => p l a b -> Traversal' a b
pathed = unT . composeMap (\l -> T (toTraversal l))

newtype I (p :: (* -> * -> *) -> (* -> * -> *)) (l :: * -> * -> *) (a :: *) (b :: *) = I a
type role I nominal nominal representational nominal

type instance Index (I p l a b) = p l a b
type instance IxValue (I p l a b) = b
instance (IsPath p, ToTraversal l) => Ixed (I p l a b) where
  ix p f (I a) = I <$> pathed p f a