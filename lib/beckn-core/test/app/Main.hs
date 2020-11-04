module Main where

import Amount
import DecimalValue
import EulerHS.Prelude
import SignatureAuth
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [amountTests, decimalValueTests, signatureAuthTests]
  return $ testGroup "Tests" [unitTests]
