module Main where

import Currency
import EulerHS.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [currencyTests]
  return $ testGroup "Tests" [unitTests]
