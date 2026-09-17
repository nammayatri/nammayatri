module Main (main) where

import Kernel.Prelude
import OfferCountersTests (offerCountersTests)
import OfferFrequencyTests (offerFrequencyTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" [testGroup "Unit tests" [offerFrequencyTests, offerCountersTests]]
