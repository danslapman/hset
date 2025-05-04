# hset: Heterogeneous sets

This package provides `HSet` datatype, which is convenient to be used as "environment" with `MonadReader`

example:

```haskell
-- Here is how we can construct HSets
sut = (1 :: Int) &# "test" &# True &# hsempty

-- It is possible to take elements by type
one = elemOfType @Int sut
test = elemOfType @String sut
true = elemOfType @Bool sut

-- .. and to get projections
p = project @'[Bool, String] sut -- True &# 1 &# HSNil
```

Now you can define something like this
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Env where

import Control.Monad.Reader
import Data.HSet

class MonadEnv p m where
  askEnv :: m p

instance (MonadReader (HSet e) m, CanProject p e) => MonadEnv (HSet p) m where
  askEnv = asks project
```

and use MonadEnv with "narrow" env in a context of MonadReader with "full" environment

This approach is inspired by [ZEnvironment](https://zio.dev/reference/contextual/zenvironment) from [ZIO](https://zio.dev/reference/core/zio/)