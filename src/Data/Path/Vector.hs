{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Path.Vector (Path) where

import Control.Category (Category)
import qualified Control.Category (Category (..))
import Data.Path.Class (IsPath (..), SplitAt (..), Uncons (..), Unsnoc (..))
import Data.Type.Equality ((:~:) (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype Path (l :: k -> k -> *) (a :: k) (b :: k) = Path (Vector Any)

type role Path nominal nominal nominal

instance Category (Path l) where
  id = Path Vector.empty
  (.) (Path bc) (Path ab) = Path (ab <> bc)

instance IsPath Path where
  cons ab (Path bc) = Path (Vector.cons (unsafeCoerce ab :: Any) bc)

  uncons :: forall l a b. Path l a b -> Uncons Path l a b
  uncons (Path ab) =
    case Vector.uncons ab of
      Nothing ->
        case unsafeCoerce Refl :: a :~: b of
          Refl ->
            UnconsEmpty
      Just (ax, xb) ->
        UnconsSome (unsafeCoerce ax :: l a x) (Path xb :: Path l x b)

  snoc (Path ab) bc = Path (Vector.snoc ab (unsafeCoerce bc :: Any))

  composeR :: forall l r a b c. (forall x y z. l x y -> r y z -> r x z) -> Path l a b -> r b c -> r a c
  composeR f (Path ab) zbc =
    unsafeCoerce
      ( Vector.foldr
          (\el rest -> unsafeCoerce (f (unsafeCoerce el :: l x y) (unsafeCoerce rest :: r y z)) :: Any)
          (unsafeCoerce zbc :: Any)
          ab
      ) ::
      r a c

  composeL :: forall l r a b c. (forall x y z. r x y -> l y z -> r x z) -> r a b -> Path l b c -> r a c
  composeL f zab (Path bc) =
    unsafeCoerce
      ( Vector.foldl'
          (\acc el -> unsafeCoerce (f (unsafeCoerce acc :: r x y) (unsafeCoerce el :: l y z)) :: Any)
          (unsafeCoerce zab :: Any)
          bc
      ) ::
      r a c

  unsnoc :: forall l a b. Path l a b -> Unsnoc Path l a b
  unsnoc (Path ab) =
    case Vector.unsnoc ab of
      Nothing ->
        case unsafeCoerce Refl :: a :~: b of
          Refl ->
            UnsnocEmpty
      Just (ax, xb) ->
        UnsnocSome (Path ax :: Path l a x) (unsafeCoerce xb :: l x b)

  splitAt :: forall l a b. Int -> Path l a b -> SplitAt Path l a b
  splitAt n (Path ab) =
    let (ax, xb) = Vector.splitAt n ab
     in SplitAt (Path ax :: Path l a x) (Path xb :: Path l x b)