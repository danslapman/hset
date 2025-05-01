{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.HSet (HSet(), hsempty, (&#), Length, ContainsType, cardinality, NthType, NthElem(..), ElemIndex, HasElem(..)) where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import qualified Fcf

data HSet (ts :: [Type]) where
  HSNil :: HSet '[]
  HSCons :: t -> HSet ts -> HSet (t ': ts)

hsempty :: HSet '[]
hsempty = HSNil

(&#) :: forall t ts. (ContainsType t ts ~ 'False) => t -> HSet ts -> HSet (t ': ts)
(&#) = HSCons
infixr 5 &#

type family Length (ts :: [Type]) :: Nat where
  Length '[] = 0
  Length (t ': ts) = 1 + Length ts

type ContainsType (t :: Type) (ts :: [Type]) =
  Fcf.Eval (Fcf.IsJust Fcf.=<< Fcf.Find (Fcf.TyEq t) ts)

cardinality :: HSet ts -> Int
cardinality HSNil = 0
cardinality (HSCons _ tx) = 1 + cardinality tx

type family NthType (n :: Nat) (ts :: [Type]) where
  NthType _ '[] = TypeError ('Text "Index out of bounds")
  NthType 0 (h ': t) = h
  NthType n (h ': t) = NthType (n - 1) t

class NthElem (n :: Nat) (ts :: [Type]) where
  nthElem :: Proxy n -> HSet ts -> NthType n ts

instance {-# OVERLAPPING #-} NthElem 0 (h ': t) where
  nthElem _ (HSCons hx _) = hx

instance (1 <= n, NthElem (n - 1) t) => NthElem n (h ': t) where
  nthElem _ (HSCons _ tx) = unsafeCoerce $ nthElem (Proxy :: Proxy (n - 1)) tx

type ElemIndex (t :: Type) (ts :: [Type]) =
  Fcf.Eval (Fcf.FromMaybe Fcf.Stuck Fcf.=<< Fcf.FindIndex (Fcf.TyEq t) ts)

class HasElem (t :: Type) (ts :: [Type]) where
  elemOfType :: HSet ts -> NthType (ElemIndex t ts) ts

instance (NthElem (ElemIndex t ts) ts) => HasElem t ts where
  elemOfType = nthElem (Proxy :: Proxy (ElemIndex t ts))