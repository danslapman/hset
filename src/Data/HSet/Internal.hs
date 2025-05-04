{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.HSet.Internal where

import Data.Kind (Constraint, Type)

type family All (f :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  All f '[] = ()
  All f (x ': xs) = (f x, All f xs)