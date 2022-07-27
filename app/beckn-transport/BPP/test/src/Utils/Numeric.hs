module Utils.Numeric where

import Test.Hspec
import Prelude

checkTolerance :: (Ord a, Show a, Fractional a) => a -> a -> a -> Expectation
checkTolerance tol val valIdeal =
  abs (val - valIdeal) / valIdeal `shouldSatisfy` (< tol)
