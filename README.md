# hset: Heterogeneous sets

This package provides `HSet` datatype, which is convenient to be used as "environment" with `MonadReader`

example:

```haskell
sut = (1 :: Int) &# "test" &# True &# hsempty

one = elemOfType @Int sut
test = elemOfType @String sut
true = elemOfType @Bool sut
```

Also see tests