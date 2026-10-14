module Main (main) where

import qualified FleetPoolingPrioritySpec
import qualified FleetTagsSpec
import Kernel.Prelude
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  FleetPoolingPrioritySpec.spec
  FleetTagsSpec.spec
