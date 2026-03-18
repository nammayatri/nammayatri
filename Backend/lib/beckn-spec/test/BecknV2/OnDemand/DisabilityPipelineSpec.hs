-- | Integration-style tests for the disability tag pipeline.
--
-- These tests simulate the full flow:
--   BAP (mkDisabilityTagGroups) → Beckn wire format → BPP (buildDisabilityTag extraction)
--
-- The extraction logic mirrors 'buildDisabilityTag' from the BPP Search.hs:
--   1. Try each per-type disability group (DISABILITY_VIS, _HEA, … all 13) via asum
--   2. Fall back to legacy CUSTOMER_DISABILITY tag under CUSTOMER_INFO
--
-- If these tests break, it means the BAP→BPP disability pipeline is broken.
module BecknV2.OnDemand.DisabilityPipelineSpec (spec) where

import BecknV2.OnDemand.Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTagV2)
import Data.List (nub)
import qualified Data.Text as T
import Kernel.Prelude
import Test.Hspec

-- | All 13 ONDC v2.1.0 disability tag groups that the BPP must check.
-- This must match the disabilityGroups list in BPP Search.hs.
allDisabilityGroups :: [BecknTagGroup]
allDisabilityGroups =
  [ DISABILITY_VIS,
    DISABILITY_HEA,
    DISABILITY_MOB,
    DISABILITY_COG,
    DISABILITY_OTH,
    DISABILITY_LEP,
    DISABILITY_SPE,
    DISABILITY_INTEL,
    DISABILITY_MENTAL,
    DISABILITY_BLOOD,
    DISABILITY_DWARFISM,
    DISABILITY_ACID_ATTACK,
    DISABILITY_MULTIPLE_DIS
  ]

-- | All disability tag strings and their expected Beckn group.
disabilityTypes :: [(T.Text, BecknTagGroup)]
disabilityTypes =
  [ ("BLIND_LOW_VISION", DISABILITY_VIS),
    ("HEAR_IMPAIRMENT", DISABILITY_HEA),
    ("LOCOMOTOR_DISABILITY", DISABILITY_MOB),
    ("COGNITIVE_DISABILITY", DISABILITY_COG),
    ("LEPROSY_CURED", DISABILITY_LEP),
    ("SPEECH_LANGUAGE", DISABILITY_SPE),
    ("INTELLECTUAL_DISABILITY", DISABILITY_INTEL),
    ("MENTAL_ILLNESS", DISABILITY_MENTAL),
    ("BLOOD_DISORDER", DISABILITY_BLOOD),
    ("DWARFISM", DISABILITY_DWARFISM),
    ("ACID_ATTACK_SURVIVOR", DISABILITY_ACID_ATTACK),
    ("MULTIPLE_DISABILITIES", DISABILITY_MULTIPLE_DIS),
    ("OTHER_DISABILITY", DISABILITY_OTH)
  ]

-- | Simulate the BPP's buildDisabilityTag extraction logic.
-- This mirrors the actual code in BPP Search.hs:
--   let disabilityGroups = [Tag.DISABILITY_VIS, ..., Tag.DISABILITY_MULTIPLE_DIS]
--       fromNewGroups = asum $ map (\grp -> Utils.getTagV2 grp Tag.DISABILITY_TYPE tagGroups) disabilityGroups
--   fromNewGroups <|> Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_DISABILITY tagGroups
extractDisabilityTag :: Maybe [Spec.TagGroup] -> Maybe T.Text
extractDisabilityTag tagGroups =
  let fromNewGroups = asum $ map (\grp -> getTagV2 grp DISABILITY_TYPE tagGroups) allDisabilityGroups
   in fromNewGroups <|> getTagV2 CUSTOMER_INFO CUSTOMER_DISABILITY tagGroups

-- | Build a mock person TagGroup list as the BAP would serialize it.
mkBapPersonTags :: T.Text -> Maybe [Spec.TagGroup]
mkBapPersonTags disabilityTag = Just $ mkDisabilityTagGroups (Just disabilityTag)

-- | Build legacy CUSTOMER_DISABILITY tag group (pre-ONDC v2.1.0).
mkLegacyPersonTags :: T.Text -> Maybe [Spec.TagGroup]
mkLegacyPersonTags disabilityTag =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just "CUSTOMER_INFO",
                  descriptorLongDesc = Nothing,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDescriptor =
                      Just
                        Spec.Descriptor
                          { descriptorCode = Just "CUSTOMER_DISABILITY",
                            descriptorLongDesc = Nothing,
                            descriptorName = Nothing,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Nothing,
                    tagValue = Just disabilityTag
                  }
              ]
        }
    ]

spec :: Spec
spec = describe "DisabilityPipeline (BAP→BPP extraction)" $ do
  -- ==================================================================
  -- 1. BPP extraction via per-type disability groups (all 13)
  -- ==================================================================

  describe "BPP extraction from ONDC v2.1.0 per-type groups" $ do
    forM_ disabilityTypes $ \(tag, expectedGrp) -> do
      it ("extracts " <> T.unpack tag <> " from " <> show expectedGrp <> " group") $ do
        let personTags = mkBapPersonTags tag
        extractDisabilityTag personTags `shouldBe` Just tag

  -- ==================================================================
  -- 2. Legacy CUSTOMER_DISABILITY fallback
  -- ==================================================================

  describe "legacy CUSTOMER_DISABILITY fallback" $ do
    it "extracts BLIND_LOW_VISION from legacy CUSTOMER_INFO group" $ do
      let personTags = mkLegacyPersonTags "BLIND_LOW_VISION"
      extractDisabilityTag personTags `shouldBe` Just "BLIND_LOW_VISION"

    it "extracts LOCOMOTOR_DISABILITY from legacy CUSTOMER_INFO group" $ do
      let personTags = mkLegacyPersonTags "LOCOMOTOR_DISABILITY"
      extractDisabilityTag personTags `shouldBe` Just "LOCOMOTOR_DISABILITY"

    it "prefers new per-type group over legacy when both present" $ do
      let newTags = mkDisabilityTagGroups (Just "BLIND_LOW_VISION")
      let legacyTag =
            Spec.TagGroup
              { tagGroupDescriptor =
                  Just
                    Spec.Descriptor
                      { descriptorCode = Just "CUSTOMER_INFO",
                        descriptorLongDesc = Nothing,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagGroupDisplay = Just False,
                tagGroupList =
                  Just
                    [ Spec.Tag
                        { tagDescriptor =
                            Just
                              Spec.Descriptor
                                { descriptorCode = Just "CUSTOMER_DISABILITY",
                                  descriptorLongDesc = Nothing,
                                  descriptorName = Nothing,
                                  descriptorShortDesc = Nothing
                                },
                          tagDisplay = Nothing,
                          tagValue = Just "OLD_VALUE"
                        }
                    ]
              }
      let personTags = Just (newTags <> [legacyTag])
      extractDisabilityTag personTags `shouldBe` Just "BLIND_LOW_VISION"

  -- ==================================================================
  -- 3. No disability tag
  -- ==================================================================

  describe "no disability tag" $ do
    it "returns Nothing when no disability tags present" $ do
      extractDisabilityTag (Just []) `shouldBe` Nothing

    it "returns Nothing for Nothing tag groups" $ do
      extractDisabilityTag Nothing `shouldBe` Nothing

    it "returns Nothing when only unrelated tags present" $ do
      let otherTags =
            Just
              [ Spec.TagGroup
                  { tagGroupDescriptor =
                      Just
                        Spec.Descriptor
                          { descriptorCode = Just "CUSTOMER_INFO",
                            descriptorLongDesc = Nothing,
                            descriptorName = Nothing,
                            descriptorShortDesc = Nothing
                          },
                    tagGroupDisplay = Just False,
                    tagGroupList =
                      Just
                        [ Spec.Tag
                            { tagDescriptor =
                                Just
                                  Spec.Descriptor
                                    { descriptorCode = Just "CUSTOMER_LANGUAGE",
                                      descriptorLongDesc = Nothing,
                                      descriptorName = Nothing,
                                      descriptorShortDesc = Nothing
                                    },
                              tagDisplay = Nothing,
                              tagValue = Just "ENGLISH"
                            }
                        ]
                  }
              ]
      extractDisabilityTag otherTags `shouldBe` Nothing

  -- ==================================================================
  -- 4. Full BAP→BPP pipeline for every disability type
  -- ==================================================================

  describe "full BAP→BPP pipeline roundtrip" $ do
    forM_ disabilityTypes $ \(tag, _) -> do
      it ("BAP serializes and BPP extracts " <> T.unpack tag) $ do
        -- Step 1: BAP serializes via mkDisabilityTagGroups
        let bapTagGroups = mkDisabilityTagGroups (Just tag)
        length bapTagGroups `shouldBe` 1
        -- Step 2: BPP extracts via the same logic as buildDisabilityTag
        let extracted = extractDisabilityTag (Just bapTagGroups)
        extracted `shouldBe` Just tag

  -- ==================================================================
  -- 5. disabilityGroups completeness guard
  -- ==================================================================

  describe "disabilityGroups completeness" $ do
    it "allDisabilityGroups contains exactly 13 groups" $ do
      length allDisabilityGroups `shouldBe` 13

    it "allDisabilityGroups has no duplicates" $ do
      length allDisabilityGroups `shouldBe` length (nub allDisabilityGroups)

    it "every disabilityTagToGroup result is in allDisabilityGroups" $ do
      let allTags = map fst disabilityTypes
      let groups = map disabilityTagToGroup allTags
      forM_ groups $ \grp ->
        grp `shouldSatisfy` (`elem` allDisabilityGroups)
