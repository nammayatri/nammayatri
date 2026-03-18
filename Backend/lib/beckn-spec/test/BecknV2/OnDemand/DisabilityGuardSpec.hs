-- | CI guard tests to prevent disability tag regressions.
--
-- These tests ensure that:
--   1. The disability_tag column is present in all required YAML storage specs
--   2. Every ONDC disability type survives a Beckn protocol roundtrip
--      (serialise via 'mkDisabilityTagGroups', deserialise via 'getTagV2')
--
-- If any of these tests fail, it means a change in the search/select/booking
-- pipeline has accidentally dropped or broken the disability tag.
module BecknV2.OnDemand.DisabilityGuardSpec (spec) where

import BecknV2.OnDemand.Tags
import BecknV2.Utils (getTagV2)
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Kernel.Prelude
import System.Directory (doesFileExist)
import Test.Hspec

-- | The 13 ONDC v2.1.0 disability tag strings that must roundtrip through
--   the Beckn protocol.
allDisabilityTagStrings :: [T.Text]
allDisabilityTagStrings =
  [ "BLIND_LOW_VISION",
    "HEAR_IMPAIRMENT",
    "LOCOMOTOR_DISABILITY",
    "COGNITIVE_DISABILITY",
    "LEPROSY_CURED",
    "SPEECH_LANGUAGE",
    "INTELLECTUAL_DISABILITY",
    "MENTAL_ILLNESS",
    "BLOOD_DISORDER",
    "DWARFISM",
    "ACID_ATTACK_SURVIVOR",
    "MULTIPLE_DISABILITIES",
    "OTHER_DISABILITY"
  ]

-- | Expected Beckn tag group for each disability type.
expectedGroup :: T.Text -> BecknTagGroup
expectedGroup = \case
  "BLIND_LOW_VISION" -> DISABILITY_VIS
  "HEAR_IMPAIRMENT" -> DISABILITY_HEA
  "LOCOMOTOR_DISABILITY" -> DISABILITY_MOB
  "COGNITIVE_DISABILITY" -> DISABILITY_COG
  "LEPROSY_CURED" -> DISABILITY_LEP
  "SPEECH_LANGUAGE" -> DISABILITY_SPE
  "INTELLECTUAL_DISABILITY" -> DISABILITY_INTEL
  "MENTAL_ILLNESS" -> DISABILITY_MENTAL
  "BLOOD_DISORDER" -> DISABILITY_BLOOD
  "DWARFISM" -> DISABILITY_DWARFISM
  "ACID_ATTACK_SURVIVOR" -> DISABILITY_ACID_ATTACK
  "MULTIPLE_DISABILITIES" -> DISABILITY_MULTIPLE_DIS
  _ -> DISABILITY_OTH

-- | YAML spec files that must contain a @disabilityTag@ field.  Paths are
--   relative to the repository root.  The test uses a simple substring
--   check — if the field is removed from the YAML spec, the generated
--   migration will also drop the column and the test will fail.
yamlSpecFiles :: [(String, FilePath)]
yamlSpecFiles =
  [ ("atlas_app.search_request", "app/rider-platform/rider-app/Main/spec/Storage/SearchRequest.yaml"),
    ("atlas_app.booking", "app/rider-platform/rider-app/Main/spec/Storage/Booking.yaml"),
    ("atlas_driver_offer_bpp.search_request", "app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SearchRequest.yaml"),
    ("atlas_driver_offer_bpp.booking", "app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Booking.yaml")
  ]

spec :: Spec
spec = describe "DisabilityGuard (CI regression tests)" $ do
  -- ==================================================================
  -- 1. Golden test: disability_tag column exists in YAML storage specs
  -- ==================================================================

  describe "disability_tag schema golden test" $ do
    forM_ yamlSpecFiles $ \(tableName, relPath) -> do
      it ("disabilityTag field exists in " <> tableName) $ do
        -- Compute path relative to beckn-spec/test/ — go up to Backend/
        let path = "../../" <> relPath
        exists <- doesFileExist path
        exists `shouldBe` True
        contents <- TIO.readFile path
        T.isInfixOf "disabilityTag" contents `shouldBe` True

  -- ==================================================================
  -- 2. Beckn protocol roundtrip: serialize → deserialize for all types
  -- ==================================================================

  describe "Beckn protocol roundtrip" $ do
    forM_ allDisabilityTagStrings $ \tag -> do
      it ("roundtrips " <> T.unpack tag <> " through Beckn format") $ do
        let tagGroups = mkDisabilityTagGroups (Just tag)
        -- Must produce exactly one tag group
        length tagGroups `shouldBe` 1
        -- The group must have the correct descriptor code
        let grp = head tagGroups
        let grpCode = grp.tagGroupDescriptor >>= (.descriptorCode)
        grpCode `shouldBe` Just (T.pack $ show $ expectedGroup tag)
        -- Extract the disability type back via getTagV2
        let extracted = getTagV2 (expectedGroup tag) DISABILITY_TYPE (Just tagGroups)
        extracted `shouldBe` Just tag

  -- ==================================================================
  -- 3. Exhaustiveness: all 13 types must be covered
  -- ==================================================================

  describe "exhaustiveness" $ do
    it "covers exactly 13 disability types" $ do
      length allDisabilityTagStrings `shouldBe` 13

    it "disabilityTagToGroup maps each known type to a unique group" $ do
      let groups = map disabilityTagToGroup (filter (/= "OTHER_DISABILITY") allDisabilityTagStrings)
      -- No two known types should map to the same group (OTHER_DISABILITY is the fallback)
      length groups `shouldBe` length (nub groups)

    it "mkDisabilityTagGroups returns empty for Nothing" $ do
      mkDisabilityTagGroups Nothing `shouldBe` []
