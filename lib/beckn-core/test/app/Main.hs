module Main where

import Amount
import DecimalValue
import EulerHS.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [amountTests, decimalValueTests]
  return $ testGroup "Tests" [unitTests]
