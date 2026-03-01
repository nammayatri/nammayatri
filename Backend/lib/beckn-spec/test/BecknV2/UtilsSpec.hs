module BecknV2.UtilsSpec (spec) where

import BecknV2.OnDemand.Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils
import Kernel.Prelude
import Test.Hspec

-- | Helper: build a TagGroup with a specific descriptor code and tags
mkTestTagGroup :: Text -> [(Text, Maybe Text)] -> Spec.TagGroup
mkTestTagGroup groupCode tags =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just groupCode,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList =
        if null tags
          then Nothing
          else
            Just $
              map
                ( \(code, val) ->
                    Spec.Tag
                      { tagDescriptor =
                          Just
                            Spec.Descriptor
                              { descriptorCode = Just code,
                                descriptorName = Nothing,
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Nothing,
                        tagValue = val
                      }
                )
                tags
    }

spec :: Spec
spec = describe "BecknV2.Utils" $ do
  -- ================================================================
  -- getTagV2
  -- ================================================================

  describe "getTagV2" $ do
    it "extracts tag value from matching group and tag" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")]]
      getTagV2 FARE_POLICY MIN_FARE tagGroups `shouldBe` Just "50"

    it "returns Nothing for missing group" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")]]
      getTagV2 BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Nothing

    it "returns Nothing for missing tag within group" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")]]
      getTagV2 FARE_POLICY CANCELLATION_FEE_PERCENTAGE tagGroups `shouldBe` Nothing

    it "returns Nothing for Nothing tagGroups" $ do
      getTagV2 FARE_POLICY MIN_FARE Nothing `shouldBe` Nothing

    it "returns Nothing for empty tagGroups list" $ do
      getTagV2 FARE_POLICY MIN_FARE (Just []) `shouldBe` Nothing

    it "returns Nothing when tag value is Nothing" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Nothing)]]
      getTagV2 FARE_POLICY MIN_FARE tagGroups `shouldBe` Nothing

    it "extracts from multiple groups" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")],
                mkTestTagGroup "BAP_TERMS" [("BUYER_FINDER_FEE_TYPE", Just "percent")]
              ]
      getTagV2 BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"

    it "extracts from multiple tags within a group" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50"), ("CANCELLATION_FEE_PERCENTAGE", Just "10")]]
      getTagV2 FARE_POLICY CANCELLATION_FEE_PERCENTAGE tagGroups `shouldBe` Just "10"

  -- ================================================================
  -- getTagV2Compat (backward-compatible tag parsing)
  -- ================================================================

  describe "getTagV2Compat" $ do
    it "finds value in BAP_TERMS group" $ do
      let tagGroups = Just [mkTestTagGroup "BAP_TERMS" [("BUYER_FINDER_FEE_TYPE", Just "percent")]]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"

    it "falls back to BUYER_FINDER_FEES when BAP_TERMS missing" $ do
      let tagGroups = Just [mkTestTagGroup "BUYER_FINDER_FEES" [("BUYER_FINDER_FEE_TYPE", Just "percent")]]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"

    it "finds value in BPP_TERMS group" $ do
      let tagGroups = Just [mkTestTagGroup "BPP_TERMS" [("SETTLEMENT_WINDOW", Just "P2D")]]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"

    it "falls back to SETTLEMENT_TERMS when BPP_TERMS missing" $ do
      let tagGroups = Just [mkTestTagGroup "SETTLEMENT_TERMS" [("SETTLEMENT_WINDOW", Just "P2D")]]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"

    it "returns Nothing when neither new nor legacy group exists" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")]]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Nothing

    it "prefers new group over legacy group" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BAP_TERMS" [("BUYER_FINDER_FEE_TYPE", Just "new_value")],
                mkTestTagGroup "BUYER_FINDER_FEES" [("BUYER_FINDER_FEE_TYPE", Just "old_value")]
              ]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "new_value"

    it "non-BAP/BPP groups have no fallback" $ do
      let tagGroups = Just [mkTestTagGroup "SOME_OTHER" [("MIN_FARE", Just "50")]]
      getTagV2Compat FARE_POLICY MIN_FARE tagGroups `shouldBe` Nothing

    it "returns Nothing for Nothing tagGroups" $ do
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE Nothing `shouldBe` Nothing

    it "returns Nothing for empty tagGroups list" $ do
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE (Just []) `shouldBe` Nothing

    it "falls back correctly for BPP_TERMS with multiple tags" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "SETTLEMENT_TERMS"
                  [ ("SETTLEMENT_WINDOW", Just "P2D"),
                    ("SETTLEMENT_BASIS", Just "DELIVERY")
                  ]
              ]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"
      getTagV2Compat BPP_TERMS SETTLEMENT_BASIS tagGroups `shouldBe` Just "DELIVERY"

  -- ================================================================
  -- parseISO8601Duration / formatTimeDifference
  -- ================================================================

  describe "parseISO8601Duration" $ do
    it "parses PT10M (10 minutes)" $ do
      let result = parseISO8601Duration "PT10M"
      result `shouldSatisfy` isJust
      -- 10 minutes = 600 seconds
      fromJust result `shouldBe` 600

    it "parses PT1H (1 hour)" $ do
      let result = parseISO8601Duration "PT1H"
      result `shouldSatisfy` isJust
      fromJust result `shouldBe` 3600

    it "parses PT30S (30 seconds)" $ do
      let result = parseISO8601Duration "PT30S"
      result `shouldSatisfy` isJust
      fromJust result `shouldBe` 30

    it "parses PT1H30M (1 hour 30 minutes)" $ do
      let result = parseISO8601Duration "PT1H30M"
      result `shouldSatisfy` isJust
      fromJust result `shouldBe` 5400

    it "returns Nothing for invalid string" $ do
      parseISO8601Duration "not_a_duration" `shouldBe` Nothing

    it "returns Nothing for empty string" $ do
      parseISO8601Duration "" `shouldBe` Nothing

    it "parses PT0S (zero duration)" $ do
      parseISO8601Duration "PT0S" `shouldBe` Just 0

  describe "formatTimeDifference" $ do
    it "formats 600 seconds as PT10M" $ do
      formatTimeDifference 600 `shouldBe` "PT10M"

    it "formats 3600 seconds as PT1H" $ do
      formatTimeDifference 3600 `shouldBe` "PT1H"

    it "round-trips with parseISO8601Duration" $ do
      let duration = 5400 -- 1h30m
      let formatted = formatTimeDifference duration
      let parsed = parseISO8601Duration formatted
      parsed `shouldBe` Just duration

    it "round-trips 0 seconds" $ do
      let formatted = formatTimeDifference 0
      let parsed = parseISO8601Duration formatted
      parsed `shouldBe` Just 0

  -- ================================================================
  -- maskNumber
  -- ================================================================

  describe "maskNumber" $ do
    it "masks middle digits of 10-digit number" $ do
      let result = maskNumber "9876543210"
      result `shouldBe` "98******10"

    it "returns short numbers unchanged" $ do
      maskNumber "1234" `shouldBe` "1234"

    it "masks 6-digit number" $ do
      maskNumber "123456" `shouldBe` "12**56"

    it "returns empty string unchanged" $ do
      maskNumber "" `shouldBe` ""

    it "returns 4-char string unchanged (boundary)" $ do
      maskNumber "abcd" `shouldBe` "abcd"

    it "masks 5-char string (just above boundary)" $ do
      maskNumber "12345" `shouldBe` "12*45"

  -- ================================================================
  -- mkStopInstructions (shared utility)
  -- ================================================================

  describe "mkStopInstructions" $ do
    it "returns Nothing for Nothing input" $ do
      mkStopInstructions Nothing `shouldBe` Nothing

    it "creates Descriptor for Just value" $ do
      let result = mkStopInstructions (Just "Take Gate A")
      result `shouldSatisfy` isJust
      let desc = fromJust result
      desc.descriptorName `shouldBe` Just "Take Gate A"
      desc.descriptorCode `shouldBe` Nothing
      desc.descriptorShortDesc `shouldBe` Nothing

  -- ================================================================
  -- mkScheduledPickupDuration (shared utility)
  -- ================================================================

  describe "mkScheduledPickupDuration" $ do
    it "returns Nothing for non-scheduled rides" $ do
      mkScheduledPickupDuration False `shouldBe` Nothing

    it "returns ISO 8601 duration for scheduled rides" $ do
      let result = mkScheduledPickupDuration True
      result `shouldSatisfy` isJust
      let duration = fromJust result
      -- Should be parseable and equal to 600 seconds (10 minutes)
      let parsed = parseISO8601Duration duration
      parsed `shouldBe` Just 600

    it "scheduled duration is exactly PT10M" $ do
      mkScheduledPickupDuration True `shouldBe` Just "PT10M"

  -- ================================================================
  -- Spec switching: getTagV2Compat realistic scenarios
  -- ================================================================

  describe "spec switching: getTagV2Compat realistic scenarios" $ do
    it "reads BUYER_FINDER_FEE_TYPE from v2.1.0 BAP_TERMS payload" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BAP_TERMS"
                  [ ("BUYER_FINDER_FEE_TYPE", Just "percent"),
                    ("BUYER_FINDER_FEE_AMOUNT", Just "5.0")
                  ]
              ]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_AMOUNT tagGroups `shouldBe` Just "5.0"

    it "reads BUYER_FINDER_FEE_TYPE from v2.0.0 BUYER_FINDER_FEES payload" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BUYER_FINDER_FEES"
                  [ ("BUYER_FINDER_FEE_TYPE", Just "percent"),
                    ("BUYER_FINDER_FEE_AMOUNT", Just "3.0")
                  ]
              ]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_AMOUNT tagGroups `shouldBe` Just "3.0"

    it "reads SETTLEMENT_WINDOW from v2.1.0 BPP_TERMS payload" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BPP_TERMS"
                  [ ("SETTLEMENT_WINDOW", Just "P2D"),
                    ("SETTLEMENT_BASIS", Just "DELIVERY")
                  ]
              ]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"
      getTagV2Compat BPP_TERMS SETTLEMENT_BASIS tagGroups `shouldBe` Just "DELIVERY"

    it "reads SETTLEMENT_WINDOW from v2.0.0 SETTLEMENT_TERMS payload" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "SETTLEMENT_TERMS"
                  [ ("SETTLEMENT_WINDOW", Just "P1D"),
                    ("SETTLEMENT_BASIS", Just "DELIVERY")
                  ]
              ]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P1D"
      getTagV2Compat BPP_TERMS SETTLEMENT_BASIS tagGroups `shouldBe` Just "DELIVERY"

    it "handles mixed v2.0.0 and v2.1.0 tag groups in same payload" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BAP_TERMS" [("BUYER_FINDER_FEE_TYPE", Just "percent")],
                mkTestTagGroup "SETTLEMENT_TERMS" [("SETTLEMENT_WINDOW", Just "P2D")],
                mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50")]
              ]
      -- BAP_TERMS is v2.1.0 name, should match directly
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"
      -- BPP_TERMS doesn't exist, but SETTLEMENT_TERMS does (v2.0.0 fallback)
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"
      -- FARE_POLICY has no fallback behavior, direct match
      getTagV2Compat FARE_POLICY MIN_FARE tagGroups `shouldBe` Just "50"

    it "v2.1.0 BAP_TERMS takes priority over v2.0.0 BUYER_FINDER_FEES" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BAP_TERMS" [("BUYER_FINDER_FEE_TYPE", Just "new_value")],
                mkTestTagGroup "BUYER_FINDER_FEES" [("BUYER_FINDER_FEE_TYPE", Just "old_value")]
              ]
      getTagV2Compat BAP_TERMS BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "new_value"

    it "v2.1.0 BPP_TERMS takes priority over v2.0.0 SETTLEMENT_TERMS" $ do
      let tagGroups =
            Just
              [ mkTestTagGroup "BPP_TERMS" [("SETTLEMENT_WINDOW", Just "P3D")],
                mkTestTagGroup "SETTLEMENT_TERMS" [("SETTLEMENT_WINDOW", Just "P1D")]
              ]
      getTagV2Compat BPP_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P3D"

  -- ================================================================
  -- getTagV2: existing tag groups still work (regression)
  -- ================================================================

  describe "getTagV2 existing tag groups (regression)" $ do
    it "reads from FARE_POLICY group" $ do
      let tagGroups = Just [mkTestTagGroup "FARE_POLICY" [("MIN_FARE", Just "50"), ("MAX_FARE", Just "500")]]
      getTagV2 FARE_POLICY MIN_FARE tagGroups `shouldBe` Just "50"

    it "reads from ROUTE_INFO group" $ do
      let tagGroups = Just [mkTestTagGroup "ROUTE_INFO" [("DISTANCE_INFO_IN_M", Just "5000")]]
      getTagV2 ROUTE_INFO DISTANCE_INFO_IN_M tagGroups `shouldBe` Just "5000"

    it "reads from CUSTOMER_INFO group" $ do
      let tagGroups = Just [mkTestTagGroup "CUSTOMER_INFO" [("CUSTOMER_LANGUAGE", Just "ENGLISH")]]
      getTagV2 CUSTOMER_INFO CUSTOMER_LANGUAGE tagGroups `shouldBe` Just "ENGLISH"

    it "reads from legacy BUYER_FINDER_FEES group directly" $ do
      let tagGroups = Just [mkTestTagGroup "BUYER_FINDER_FEES" [("BUYER_FINDER_FEE_TYPE", Just "percent")]]
      getTagV2 BUYER_FINDER_FEES BUYER_FINDER_FEE_TYPE tagGroups `shouldBe` Just "percent"

    it "reads from legacy SETTLEMENT_TERMS group directly" $ do
      let tagGroups = Just [mkTestTagGroup "SETTLEMENT_TERMS" [("SETTLEMENT_WINDOW", Just "P2D")]]
      getTagV2 SETTLEMENT_TERMS SETTLEMENT_WINDOW tagGroups `shouldBe` Just "P2D"

    it "reads from SEARCH_REQUEST_INFO group" $ do
      let tagGroups = Just [mkTestTagGroup "SEARCH_REQUEST_INFO" [("IS_METER_RIDE_SEARCH", Just "True")]]
      getTagV2 SEARCH_REQUEST_INFO IS_METER_RIDE_SEARCH tagGroups `shouldBe` Just "True"
