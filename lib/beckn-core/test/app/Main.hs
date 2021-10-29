module Main where

import APIExceptions
import Amount
import DecimalValue
import EulerHS.Prelude
import SignatureAuth
import SlidingWindowLimiter
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = return $ testGroup "Tests" [unitTests]
  where
    unitTests =
      testGroup
        "Unit tests"
        [ amountTests,
          decimalValueTests,
          signatureAuthTests,
          httpExceptionTests,
          slidingWindowLimiterTests
        ]
