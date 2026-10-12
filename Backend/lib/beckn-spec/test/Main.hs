module Main where

import qualified BecknV2.OnDemand.EnumsSpec as EnumsSpec
import qualified BecknV2.OnDemand.ProjectedTagsSpec as ProjectedTagsSpec
import qualified BecknV2.OnDemand.TagsSpec as TagsSpec
import qualified BecknV2.OnDemand.TypesSpec as TypesSpec
import qualified BecknV2.UtilsSpec as UtilsSpec
import Kernel.Prelude
import Test.Hspec

main :: IO ()
main = hspec $ do
  TypesSpec.spec
  EnumsSpec.spec
  ProjectedTagsSpec.spec
  TagsSpec.spec
  UtilsSpec.spec
