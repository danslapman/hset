{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.HSet
import Data.Proxy
import GHC.TypeLits
import Test.DocTest
import Test.HUnit
import qualified Fcf

type BoolToNat = Fcf.Case
  [ 'True  Fcf.--> 1
  , 'False Fcf.--> 0
  ]

type Sut = [Int, String, Bool]

lengthTest :: Test
lengthTest =
  TestCase $ assertEqual "length" 3 (natVal $ Proxy @(Length '[Int, String, Bool]))

containsTest :: Test
containsTest =
  TestCase $ assertEqual "contains" 1 (natVal $ Proxy @(Fcf.Eval (BoolToNat (ContainsType Int Sut))))

tests :: Test
tests = 
  TestList [
    lengthTest,
    containsTest
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  doctest ["-isrc", "src/Data/HSet.hs"]