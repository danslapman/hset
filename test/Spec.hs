{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.HSet
import Data.Proxy
import GHC.TypeLits
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

getNthElemTest :: Test
getNthElemTest =
  let sut = (1 :: Int) &# "test" &# True &# hsempty
  in  TestList [
    TestCase $ assertEqual "nthElem" 1 (nthElem (Proxy @0) sut),
    TestCase $ assertEqual "nthElem" "test" (nthElem (Proxy @1) sut),
    TestCase $ assertEqual "nthElem" True (nthElem (Proxy @2) sut)
  ]

elemOfTypeTest :: Test
elemOfTypeTest =
  let sut = (1 :: Int) &# "test" &# True &# hsempty
  in  TestList [
    TestCase $ assertEqual "elemOfType" 1 (elemOfType @Int sut),
    TestCase $ assertEqual "elemOfType" "test" (elemOfType @String sut),
    TestCase $ assertEqual "elemOfType" True (elemOfType @Bool sut)
  ]

getElemTest :: Test
getElemTest =
  let sut = (1 :: Int) &# "test" &# True &# hsempty
  in  TestList [
    TestCase $ assertEqual "getElem" 1 (getElem (Proxy @Int) sut),
    TestCase $ assertEqual "getElem" "test" (getElem (Proxy @String) sut),
    TestCase $ assertEqual "getElem" True (getElem (Proxy @Bool) sut)
  ]

getSubsetTest :: Test
getSubsetTest =
  let sut = (1 :: Int) &# "test" &# True &# hsempty
      expected = True &# "test" &# hsempty
  in TestCase $ assertEqual "project" expected (project sut)

tests :: Test
tests = 
  TestList [
    lengthTest,
    containsTest,
    getNthElemTest,
    elemOfTypeTest,
    getElemTest,
    getSubsetTest
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()