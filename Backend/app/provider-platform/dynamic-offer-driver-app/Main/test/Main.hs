module Main where

import Kernel.Prelude
import Test.Hspec
import qualified FareCalculatorSpec

main :: IO ()
main = hspec FareCalculatorSpec.spec
