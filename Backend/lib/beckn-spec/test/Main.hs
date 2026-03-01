module Main where

import qualified BecknV2.OnDemand.EnumsSpec as EnumsSpec
import qualified BecknV2.OnDemand.TagsSpec as TagsSpec
import qualified BecknV2.OnDemand.TypesSpec as TypesSpec
import qualified BecknV2.UtilsSpec as UtilsSpec
import Kernel.Prelude
import Test.Hspec

main :: IO ()
main = hspec $ do
  TypesSpec.spec
  EnumsSpec.spec
  TagsSpec.spec
  UtilsSpec.spec
