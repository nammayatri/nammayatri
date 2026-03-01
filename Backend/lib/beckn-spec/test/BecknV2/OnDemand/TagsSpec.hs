module BecknV2.OnDemand.TagsSpec (spec) where

import BecknV2.OnDemand.Tags
import Kernel.Prelude
import Test.Hspec

spec :: Spec
spec = describe "BecknV2.OnDemand.Tags" $ do
  -- ================================================================
  -- Tag group descriptor construction
  -- ================================================================

  describe "getTagGroupDescriptor" $ do
    it "BAP_TERMS has correct descriptor code" $ do
      let desc = getTagGroupDescriptor BAP_TERMS
      desc.descriptorCode `shouldBe` Just "BAP_TERMS"

    it "BPP_TERMS has correct descriptor code" $ do
      let desc = getTagGroupDescriptor BPP_TERMS
      desc.descriptorCode `shouldBe` Just "BPP_TERMS"

    it "FEATURE_LIST has correct descriptor code" $ do
      let desc = getTagGroupDescriptor FEATURE_LIST
      desc.descriptorCode `shouldBe` Just "FEATURE_LIST"

    it "DISABILITY_VIS has correct descriptor code" $ do
      let desc = getTagGroupDescriptor DISABILITY_VIS
      desc.descriptorCode `shouldBe` Just "DISABILITY_VIS"

    it "DISABILITY_HEA has correct descriptor code" $ do
      let desc = getTagGroupDescriptor DISABILITY_HEA
      desc.descriptorCode `shouldBe` Just "DISABILITY_HEA"

    it "DISABILITY_MOB has correct descriptor code" $ do
      let desc = getTagGroupDescriptor DISABILITY_MOB
      desc.descriptorCode `shouldBe` Just "DISABILITY_MOB"

    it "DISABILITY_COG has correct descriptor code" $ do
      let desc = getTagGroupDescriptor DISABILITY_COG
      desc.descriptorCode `shouldBe` Just "DISABILITY_COG"

    it "DISABILITY_OTH has correct descriptor code" $ do
      let desc = getTagGroupDescriptor DISABILITY_OTH
      desc.descriptorCode `shouldBe` Just "DISABILITY_OTH"

    it "BAP_TERMS has descriptive name" $ do
      let desc = getTagGroupDescriptor BAP_TERMS
      desc.descriptorName `shouldBe` Just "BAP Terms"

    it "BPP_TERMS has descriptive name" $ do
      let desc = getTagGroupDescriptor BPP_TERMS
      desc.descriptorName `shouldBe` Just "BPP Terms"

    it "legacy BUYER_FINDER_FEES still has descriptor" $ do
      let desc = getTagGroupDescriptor BUYER_FINDER_FEES
      desc.descriptorCode `shouldBe` Just "BUYER_FINDER_FEES"

  -- ================================================================
  -- Tag descriptor construction
  -- ================================================================

  describe "getFullTag / getTagDescriptor" $ do
    it "FEATURE_NAME tag has correct descriptor code" $ do
      let tag = getFullTag FEATURE_NAME (Just "AC")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "FEATURE_NAME"

    it "FEATURE_VALUE tag has correct descriptor code" $ do
      let tag = getFullTag FEATURE_VALUE (Just "true")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "FEATURE_VALUE"

    it "DISABILITY_TYPE tag has correct descriptor code" $ do
      let tag = getFullTag DISABILITY_TYPE (Just "BLIND_LOW_VISION")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "DISABILITY_TYPE"

    it "tag preserves value" $ do
      let tag = getFullTag FEATURE_NAME (Just "AC")
      tag.tagValue `shouldBe` Just "AC"

    it "tag preserves Nothing value" $ do
      let tag = getFullTag FEATURE_NAME Nothing
      tag.tagValue `shouldBe` Nothing

  -- ================================================================
  -- Tag group routing
  -- ================================================================

  describe "getTagGroup routing" $ do
    it "BUYER_FINDER_FEE_TYPE routes to BAP_TERMS" $ do
      getTagGroup BUYER_FINDER_FEE_TYPE `shouldBe` BAP_TERMS

    it "BUYER_FINDER_FEE_AMOUNT routes to BAP_TERMS" $ do
      getTagGroup BUYER_FINDER_FEE_AMOUNT `shouldBe` BAP_TERMS

    it "SETTLEMENT_WINDOW routes to BPP_TERMS" $ do
      getTagGroup SETTLEMENT_WINDOW `shouldBe` BPP_TERMS

    it "STATIC_TERMS_URL routes to BPP_TERMS" $ do
      getTagGroup STATIC_TERMS_URL `shouldBe` BPP_TERMS

    it "FEATURE_NAME routes to FEATURE_LIST" $ do
      getTagGroup FEATURE_NAME `shouldBe` FEATURE_LIST

    it "FEATURE_VALUE routes to FEATURE_LIST" $ do
      getTagGroup FEATURE_VALUE `shouldBe` FEATURE_LIST

    it "DISABILITY_TYPE routes to DISABILITY_VIS" $ do
      -- DISABILITY_TYPE defaults to DISABILITY_VIS in routing
      getTagGroup DISABILITY_TYPE `shouldBe` DISABILITY_VIS

    it "CANCELLATION_FEE_PERCENTAGE routes to FARE_POLICY" $ do
      getTagGroup CANCELLATION_FEE_PERCENTAGE `shouldBe` FARE_POLICY

  -- ================================================================
  -- convertToTagGroup
  -- ================================================================

  describe "convertToTagGroup" $ do
    it "groups tags by their tag group" $ do
      let tagList =
            [ (FEATURE_NAME, Just "AC"),
              (FEATURE_VALUE, Just "true"),
              (BUYER_FINDER_FEE_TYPE, Just "percent")
            ]
      let result = convertToTagGroup tagList
      result `shouldSatisfy` isJust
      -- Should produce 2 groups: FEATURE_LIST and BAP_TERMS
      let groups = fromJust result
      length groups `shouldBe` 2

    it "filters out Nothing values" $ do
      let tagList = [(FEATURE_NAME, Nothing), (FEATURE_VALUE, Nothing)]
      convertToTagGroup tagList `shouldBe` Nothing

    it "returns Nothing for empty list" $ do
      convertToTagGroup [] `shouldBe` Nothing

    it "keeps tags with Just values, filters Nothing" $ do
      let tagList = [(FEATURE_NAME, Just "AC"), (FEATURE_VALUE, Nothing)]
      let result = convertToTagGroup tagList
      result `shouldSatisfy` isJust
      let groups = fromJust result
      length groups `shouldBe` 1

  -- ================================================================
  -- Disability tag helpers (v2.1.0)
  -- ================================================================

  describe "disabilityTagToGroup" $ do
    it "maps BLIND_LOW_VISION to DISABILITY_VIS" $ do
      disabilityTagToGroup "BLIND_LOW_VISION" `shouldBe` DISABILITY_VIS

    it "maps HEAR_IMPAIRMENT to DISABILITY_HEA" $ do
      disabilityTagToGroup "HEAR_IMPAIRMENT" `shouldBe` DISABILITY_HEA

    it "maps LOCOMOTOR_DISABILITY to DISABILITY_MOB" $ do
      disabilityTagToGroup "LOCOMOTOR_DISABILITY" `shouldBe` DISABILITY_MOB

    it "maps COGNITIVE_DISABILITY to DISABILITY_COG" $ do
      disabilityTagToGroup "COGNITIVE_DISABILITY" `shouldBe` DISABILITY_COG

    it "maps unknown to DISABILITY_OTH" $ do
      disabilityTagToGroup "SOMETHING_ELSE" `shouldBe` DISABILITY_OTH

    it "maps empty string to DISABILITY_OTH" $ do
      disabilityTagToGroup "" `shouldBe` DISABILITY_OTH

  describe "mkDisabilityTagGroups" $ do
    it "returns empty list for Nothing" $ do
      mkDisabilityTagGroups Nothing `shouldBe` []

    it "creates tag group for BLIND_LOW_VISION" $ do
      let groups = mkDisabilityTagGroups (Just "BLIND_LOW_VISION")
      length groups `shouldBe` 1
      let grp = head groups
      -- Check descriptor code is DISABILITY_VIS
      (grp.tagGroupDescriptor >>= (.descriptorCode)) `shouldBe` Just "DISABILITY_VIS"

    it "creates tag group with DISABILITY_TYPE tag inside" $ do
      let groups = mkDisabilityTagGroups (Just "HEAR_IMPAIRMENT")
      length groups `shouldBe` 1
      let grp = head groups
      (grp.tagGroupDescriptor >>= (.descriptorCode)) `shouldBe` Just "DISABILITY_HEA"
      -- Check there's a tag with code DISABILITY_TYPE
      let tags = fromMaybe [] grp.tagGroupList
      length tags `shouldBe` 1
      let tag = head tags
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "DISABILITY_TYPE"
      tag.tagValue `shouldBe` Just "HEAR_IMPAIRMENT"

  -- ================================================================
  -- getFullTagGroup construction
  -- ================================================================

  describe "getFullTagGroup" $ do
    it "creates TagGroup with correct descriptor" $ do
      let tags = [getFullTag FEATURE_NAME (Just "AC")]
      let grp = getFullTagGroup FEATURE_LIST tags
      (grp.tagGroupDescriptor >>= (.descriptorCode)) `shouldBe` Just "FEATURE_LIST"

    it "creates TagGroup with tags inside" $ do
      let tags = [getFullTag FEATURE_NAME (Just "AC"), getFullTag FEATURE_VALUE (Just "true")]
      let grp = getFullTagGroup FEATURE_LIST tags
      (grp.tagGroupList >>= \xs -> Just (length xs)) `shouldBe` Just 2

    it "creates TagGroup with Nothing list for empty tags" $ do
      let grp = getFullTagGroup FEATURE_LIST []
      grp.tagGroupList `shouldBe` Nothing

  -- ================================================================
  -- Show instances for BecknTagGroup
  -- ================================================================

  describe "BecknTagGroup show instances" $ do
    it "BAP_TERMS shows as \"BAP_TERMS\"" $ do
      (show BAP_TERMS :: String) `shouldBe` "BAP_TERMS"

    it "BPP_TERMS shows as \"BPP_TERMS\"" $ do
      (show BPP_TERMS :: String) `shouldBe` "BPP_TERMS"

    it "FEATURE_LIST shows as \"FEATURE_LIST\"" $ do
      (show FEATURE_LIST :: String) `shouldBe` "FEATURE_LIST"

  -- ================================================================
  -- Existing (pre-v2.1.0) tag groups: regression tests
  -- ================================================================

  describe "existing tag groups (regression)" $ do
    it "FARE_POLICY has correct descriptor code" $ do
      let desc = getTagGroupDescriptor FARE_POLICY
      desc.descriptorCode `shouldBe` Just "FARE_POLICY"

    it "INFO has correct descriptor code" $ do
      let desc = getTagGroupDescriptor INFO
      desc.descriptorCode `shouldBe` Just "INFO"

    it "ROUTE_INFO has correct descriptor code" $ do
      let desc = getTagGroupDescriptor ROUTE_INFO
      desc.descriptorCode `shouldBe` Just "ROUTE_INFO"

    it "CUSTOMER_INFO has correct descriptor code" $ do
      let desc = getTagGroupDescriptor CUSTOMER_INFO
      desc.descriptorCode `shouldBe` Just "CUSTOMER_INFO"

    it "legacy BUYER_FINDER_FEES still works" $ do
      let desc = getTagGroupDescriptor BUYER_FINDER_FEES
      desc.descriptorCode `shouldBe` Just "BUYER_FINDER_FEES"

    it "legacy SETTLEMENT_TERMS still works" $ do
      let desc = getTagGroupDescriptor SETTLEMENT_TERMS
      desc.descriptorCode `shouldBe` Just "SETTLEMENT_TERMS"

  describe "existing tags (regression)" $ do
    it "MIN_FARE tag has correct descriptor" $ do
      let tag = getFullTag MIN_FARE (Just "50")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "MIN_FARE"
      tag.tagValue `shouldBe` Just "50"

    it "DISTANCE_INFO_IN_M tag has correct descriptor" $ do
      let tag = getFullTag DISTANCE_INFO_IN_M (Just "5000")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "DISTANCE_INFO_IN_M"

    it "DURATION_INFO_IN_S tag has correct descriptor" $ do
      let tag = getFullTag DURATION_INFO_IN_S (Just "600")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "DURATION_INFO_IN_S"

    it "CUSTOMER_DISABILITY tag has correct descriptor" $ do
      let tag = getFullTag CUSTOMER_DISABILITY (Just "BLIND_LOW_VISION")
      (tag.tagDescriptor >>= (.descriptorCode)) `shouldBe` Just "CUSTOMER_DISABILITY"

  -- ================================================================
  -- Spec switching: legacy vs new tag group names
  -- ================================================================

  describe "spec switching: tag group name migration" $ do
    it "BAP_TERMS descriptor code differs from BUYER_FINDER_FEES" $ do
      let bapDesc = getTagGroupDescriptor BAP_TERMS
      let bffDesc = getTagGroupDescriptor BUYER_FINDER_FEES
      bapDesc.descriptorCode `shouldNotBe` bffDesc.descriptorCode

    it "BPP_TERMS descriptor code differs from SETTLEMENT_TERMS" $ do
      let bppDesc = getTagGroupDescriptor BPP_TERMS
      let stDesc = getTagGroupDescriptor SETTLEMENT_TERMS
      bppDesc.descriptorCode `shouldNotBe` stDesc.descriptorCode

    it "BUYER_FINDER_FEE_TYPE routes to BAP_TERMS (v2.1.0 group)" $ do
      getTagGroup BUYER_FINDER_FEE_TYPE `shouldBe` BAP_TERMS

    it "SETTLEMENT_WINDOW routes to BPP_TERMS (v2.1.0 group)" $ do
      getTagGroup SETTLEMENT_WINDOW `shouldBe` BPP_TERMS

    it "SETTLEMENT_BASIS routes to BPP_TERMS (v2.1.0 group)" $ do
      getTagGroup SETTLEMENT_BASIS `shouldBe` BPP_TERMS

    it "DELAY_INTEREST routes to BPP_TERMS (v2.1.0 group)" $ do
      getTagGroup DELAY_INTEREST `shouldBe` BPP_TERMS

    it "MANDATORY_ARBITRATION routes to BPP_TERMS (v2.1.0 group)" $ do
      getTagGroup MANDATORY_ARBITRATION `shouldBe` BPP_TERMS

    it "COURT_JURISDICTION routes to BPP_TERMS (v2.1.0 group)" $ do
      getTagGroup COURT_JURISDICTION `shouldBe` BPP_TERMS

  describe "convertToTagGroup with spec switching" $ do
    it "v2.1.0 BAP_TERMS tags group correctly" $ do
      let tagList =
            [ (BUYER_FINDER_FEE_TYPE, Just "percent"),
              (BUYER_FINDER_FEE_AMOUNT, Just "5.0")
            ]
      let result = convertToTagGroup tagList
      result `shouldSatisfy` isJust
      let groups = fromJust result
      length groups `shouldBe` 1
      ((head groups).tagGroupDescriptor >>= (.descriptorCode)) `shouldBe` Just "BAP_TERMS"

    it "v2.1.0 BPP_TERMS tags group correctly" $ do
      let tagList =
            [ (SETTLEMENT_WINDOW, Just "P2D"),
              (SETTLEMENT_BASIS, Just "DELIVERY"),
              (DELAY_INTEREST, Just "0.05")
            ]
      let result = convertToTagGroup tagList
      result `shouldSatisfy` isJust
      let groups = fromJust result
      length groups `shouldBe` 1
      ((head groups).tagGroupDescriptor >>= (.descriptorCode)) `shouldBe` Just "BPP_TERMS"

    it "mixed v2.0.0 and v2.1.0 tags group separately" $ do
      let tagList =
            [ (BUYER_FINDER_FEE_TYPE, Just "percent"),
              (FEATURE_NAME, Just "AC"),
              (FEATURE_VALUE, Just "true")
            ]
      let result = convertToTagGroup tagList
      result `shouldSatisfy` isJust
      let groups = fromJust result
      -- Should produce 2 groups: BAP_TERMS and FEATURE_LIST
      length groups `shouldBe` 2
