{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.HSet (HSet(), hsempty, (&#), Length, ContainsType, cardinality, NthType, NthElem(..), ElemIndex, HasElem(..), GetElem(..), CanProject(..)) where

import Data.HSet.Internal
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
  -- | Gets nth element
  --
  -- Examples:
  --
  -- >>> :set -XDataKinds
  -- >>> nthElem (Proxy @0) $ (1 :: Int) &# "test" &# True &# hsempty
  -- 1
  -- 
  -- >>> nthElem (Proxy @1) $ (1 :: Int) &# "test" &# True &# hsempty
  -- "test"
  --
  -- >>> nthElem (Proxy @2) $ (1 :: Int) &# "test" &# True &# hsempty
  -- True
  nthElem :: Proxy n -> HSet ts -> NthType n ts

instance {-# OVERLAPPING #-} NthElem 0 (h ': t) where
  nthElem _ (HSCons hx _) = hx

instance (1 <= n, NthElem (n - 1) t) => NthElem n (h ': t) where
  nthElem _ (HSCons _ tx) = unsafeCoerce $ nthElem (Proxy :: Proxy (n - 1)) tx

type ElemIndex (t :: Type) (ts :: [Type]) =
  Fcf.Eval (Fcf.FromMaybe Fcf.Stuck Fcf.=<< Fcf.FindIndex (Fcf.TyEq t) ts)

class HasElem (t :: Type) (ts :: [Type]) where
  -- | Gets element by type
  -- | Convenient for type apllications
  -- 
  -- Examples:
  -- 
  -- >>> :set -XDataKinds
  -- >>> elemOfType @Int $ (1 :: Int) &# "test" &# True &# hsempty
  -- 1
  -- 
  -- >>> elemOfType @String $ (1 :: Int) &# "test" &# True &# hsempty
  -- "test"
  -- 
  -- >>> elemOfType @Bool $ (1 :: Int) &# "test" &# True &# hsempty
  -- True
  elemOfType :: HSet ts -> NthType (ElemIndex t ts) ts

instance (NthElem (ElemIndex t ts) ts) => HasElem t ts where
  elemOfType = nthElem (Proxy :: Proxy (ElemIndex t ts))

class GetElem (ts :: [Type]) (t :: Type) where
  -- | Alternative to `elemOfType` for cases where type application is impossible
  --
  -- Examples:
  -- 
  -- >>> :set -XDataKinds
  -- >>> getElem (Proxy @Int) $ (1 :: Int) &# "test" &# True &# hsempty
  -- 1
  -- 
  -- >>> getElem (Proxy @String) $ (1 :: Int) &# "test" &# True &# hsempty
  -- "test"
  -- 
  -- >>> getElem (Proxy @Bool) $ (1 :: Int) &# "test" &# True &# hsempty
  -- True
  getElem :: Proxy t -> HSet ts -> NthType (ElemIndex t ts) ts

instance (NthElem (ElemIndex t ts) ts) => GetElem ts t where
  getElem _ = nthElem (Proxy :: Proxy (ElemIndex t ts))

type family Proxies (ts :: [Type]) = proxies | proxies -> ts where
  Proxies '[] = '[]
  Proxies (t ': ts) = Proxy t ': Proxies ts

class MatProxies (ts :: [Type]) where
  proxies :: HSet (Proxies ts)

instance MatProxies '[] where
  proxies = hsempty

instance (MatProxies ts) => MatProxies (t ': ts) where
  proxies = HSCons (Proxy :: Proxy t) (proxies :: HSet (Proxies ts))

class CanProject (proj :: [Type]) (src :: [Type]) where
  -- | Computes a subset of `src` for types in `proj`
  -- 
  -- Examples:
  -- 
  -- >>> :set -XDataKinds
  -- >>> project @'[Bool, String] $ (1 :: Int) &# "test" &# True &# hsempty
  -- True &# "test" &# HNil
  project :: HSet src -> HSet proj

instance CanProject '[] sx where
  project _ = hsempty

instance (GetElem src ph, CanProject pt src) => CanProject (ph ': pt) src where
  project src = HSCons (unsafeCoerce (getElem (Proxy :: Proxy ph) src)) (project src :: HSet pt)

instance All Show ts => Show (HSet ts) where
  show HSNil = "HNil"
  show (HSCons x xs) = show x ++ " &# " ++ show xs

instance All Eq ts => Eq (HSet ts) where
  HSNil == HSNil = True
  HSCons x xs == HSCons y ys = x == y && xs == ys