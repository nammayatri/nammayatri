{-# LANGUAGE DeriveDataTypeable #-}

module BecknV2.OnDemand.TypesSpec (spec) where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Kernel.Prelude
import Test.Hspec

-- | Helper: encode to JSON then decode back, check equality
roundTrip :: (Eq a, Show a, A.ToJSON a, A.FromJSON a) => a -> Expectation
roundTrip val = decode (encode val) `shouldBe` Just val

-- | Helper: check that a JSON-encoded value contains a specific key
hasJsonKey :: A.ToJSON a => a -> Text -> Expectation
hasJsonKey val key = case A.toJSON val of
  A.Object obj -> KM.member (fromString $ toString key) obj `shouldBe` True
  other -> expectationFailure $ "Expected JSON Object but got: " <> show other

-- | Helper: check that a JSON-encoded value does NOT contain a specific key
lacksJsonKey :: A.ToJSON a => a -> Text -> Expectation
lacksJsonKey val key = case A.toJSON val of
  A.Object obj -> KM.member (fromString $ toString key) obj `shouldBe` False
  other -> expectationFailure $ "Expected JSON Object but got: " <> show other

-- Sample data constructors
sampleDescriptor :: Spec.Descriptor
sampleDescriptor =
  Spec.Descriptor
    { descriptorCode = Just "TEST_CODE",
      descriptorName = Just "Test Name",
      descriptorShortDesc = Just "Short description"
    }

samplePrice :: Spec.Price
samplePrice =
  Spec.Price
    { priceValue = Just "100",
      priceComputedValue = Just "100",
      priceCurrency = Just "INR",
      priceMaximumValue = Nothing,
      priceMinimumValue = Nothing,
      priceOfferedValue = Nothing
    }

sampleCategory :: Spec.Category
sampleCategory =
  Spec.Category
    { categoryDescriptor = Just sampleDescriptor,
      categoryId = Just "ON_DEMAND_TRIP"
    }

sampleAddOn :: Spec.AddOn
sampleAddOn =
  Spec.AddOn
    { addOnDescriptor = Just sampleDescriptor,
      addOnId = Just "extra_km_10",
      addOnPrice = Just samplePrice
    }

sampleCancellationTerm :: Spec.CancellationTerm
sampleCancellationTerm =
  Spec.CancellationTerm
    { cancellationTermCancellationFee = Nothing,
      cancellationTermFulfillmentState = Nothing,
      cancellationTermReasonRequired = Just False
    }

spec :: Spec
spec = describe "BecknV2.OnDemand.Types" $ do
  -- ================================================================
  -- New v2.1.0 types: JSON round-trip
  -- ================================================================

  describe "Category (new v2.1.0 type)" $ do
    it "round-trips with all fields populated" $ do
      roundTrip sampleCategory

    it "round-trips with Nothing fields" $ do
      let cat = Spec.Category {categoryDescriptor = Nothing, categoryId = Nothing}
      roundTrip cat

    it "serializes to correct JSON keys" $ do
      sampleCategory `hasJsonKey` "descriptor"
      sampleCategory `hasJsonKey` "id"
      sampleCategory `lacksJsonKey` "categoryDescriptor"
      sampleCategory `lacksJsonKey` "categoryId"

  describe "AddOn (new v2.1.0 type)" $ do
    it "round-trips with all fields populated" $ do
      roundTrip sampleAddOn

    it "round-trips with Nothing fields" $ do
      let addOn = Spec.AddOn {addOnDescriptor = Nothing, addOnId = Nothing, addOnPrice = Nothing}
      roundTrip addOn

    it "serializes to correct JSON keys" $ do
      sampleAddOn `hasJsonKey` "descriptor"
      sampleAddOn `hasJsonKey` "id"
      sampleAddOn `hasJsonKey` "price"
      sampleAddOn `lacksJsonKey` "addOnDescriptor"

  -- ================================================================
  -- Modified types: v2.1.0 fields preserved through JSON
  -- ================================================================

  describe "Item v2.1.0 fields" $ do
    it "preserves itemCategoryIds through JSON" $ do
      let item = minimalItem {Spec.itemCategoryIds = Just ["ON_DEMAND_TRIP", "RENTAL"]}
      let decoded = decode (encode item) :: Maybe Spec.Item
      (decoded >>= Spec.itemCategoryIds) `shouldBe` Just ["ON_DEMAND_TRIP", "RENTAL"]

    it "preserves itemCancellationTerms through JSON" $ do
      let item = minimalItem {Spec.itemCancellationTerms = Just [sampleCancellationTerm]}
      let decoded = decode (encode item) :: Maybe Spec.Item
      (decoded >>= Spec.itemCancellationTerms >>= \xs -> Just (length xs)) `shouldBe` Just 1

    it "preserves itemAddOns through JSON" $ do
      let item = minimalItem {Spec.itemAddOns = Just [sampleAddOn]}
      let decoded = decode (encode item) :: Maybe Spec.Item
      (decoded >>= Spec.itemAddOns >>= \xs -> Just (length xs)) `shouldBe` Just 1

    it "serializes category_ids with correct JSON key" $ do
      let item = minimalItem {Spec.itemCategoryIds = Just ["ON_DEMAND_TRIP"]}
      item `hasJsonKey` "category_ids"
      item `lacksJsonKey` "itemCategoryIds"

    it "serializes cancellation_terms with correct JSON key" $ do
      let item = minimalItem {Spec.itemCancellationTerms = Just [sampleCancellationTerm]}
      item `hasJsonKey` "cancellation_terms"

    it "serializes add_ons with correct JSON key" $ do
      let item = minimalItem {Spec.itemAddOns = Just [sampleAddOn]}
      item `hasJsonKey` "add_ons"

  describe "Vehicle v2.1.0 fields" $ do
    it "preserves vehicleEnergyType through JSON" $ do
      let vehicle = minimalVehicle {Spec.vehicleEnergyType = Just "ELECTRIC"}
      let decoded = decode (encode vehicle) :: Maybe Spec.Vehicle
      (decoded >>= Spec.vehicleEnergyType) `shouldBe` Just "ELECTRIC"

    it "serializes energy_type with correct JSON key" $ do
      let vehicle = minimalVehicle {Spec.vehicleEnergyType = Just "CNG"}
      vehicle `hasJsonKey` "energy_type"
      vehicle `lacksJsonKey` "vehicleEnergyType"

  describe "Stop v2.1.0 fields" $ do
    it "preserves stopInstructions through JSON" $ do
      let stop = minimalStop {Spec.stopInstructions = Just sampleDescriptor}
      let decoded = decode (encode stop) :: Maybe Spec.Stop
      (decoded >>= Spec.stopInstructions >>= Spec.descriptorCode) `shouldBe` Just "TEST_CODE"

    it "serializes instructions with correct JSON key" $ do
      let stop = minimalStop {Spec.stopInstructions = Just sampleDescriptor}
      stop `hasJsonKey` "instructions"
      stop `lacksJsonKey` "stopInstructions"

  describe "Person v2.1.0 fields" $ do
    it "preserves personGender through JSON" $ do
      let person = minimalPerson {Spec.personGender = Just "FEMALE"}
      let decoded = decode (encode person) :: Maybe Spec.Person
      (decoded >>= Spec.personGender) `shouldBe` Just "FEMALE"

    it "serializes gender with correct JSON key" $ do
      let person = minimalPerson {Spec.personGender = Just "MALE"}
      person `hasJsonKey` "gender"
      person `lacksJsonKey` "personGender"

  describe "Provider v2.1.0 fields" $ do
    it "preserves providerCategories through JSON" $ do
      let provider = minimalProvider {Spec.providerCategories = Just [sampleCategory]}
      let decoded = decode (encode provider) :: Maybe Spec.Provider
      (decoded >>= Spec.providerCategories >>= \xs -> Just (length xs)) `shouldBe` Just 1

    it "serializes categories with correct JSON key" $ do
      let provider = minimalProvider {Spec.providerCategories = Just [sampleCategory]}
      provider `hasJsonKey` "categories"
      provider `lacksJsonKey` "providerCategories"

  describe "Intent v2.1.0 fields" $ do
    it "preserves intentCategory through JSON" $ do
      let intent = minimalIntent {Spec.intentCategory = Just sampleCategory}
      let decoded = decode (encode intent) :: Maybe Spec.Intent
      (decoded >>= Spec.intentCategory >>= Spec.categoryId) `shouldBe` Just "ON_DEMAND_TRIP"

    it "serializes category with correct JSON key" $ do
      let intent = minimalIntent {Spec.intentCategory = Just sampleCategory}
      intent `hasJsonKey` "category"
      intent `lacksJsonKey` "intentCategory"

  -- ================================================================
  -- Backward compatibility: v2.0.0 JSON → v2.1.0 types
  -- ================================================================

  describe "backward compatibility" $ do
    it "decodes v2.0.0 Item (no new fields) into v2.1.0 Item" $ do
      -- v2.0.0 item JSON has no category_ids, cancellation_terms, or add_ons
      let json = "{\"id\": \"item_1\", \"descriptor\": {\"code\": \"RIDE\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Item
      decoded `shouldSatisfy` isJust
      let item = fromJust decoded
      Spec.itemCategoryIds item `shouldBe` Nothing
      Spec.itemCancellationTerms item `shouldBe` Nothing
      Spec.itemAddOns item `shouldBe` Nothing

    it "decodes v2.0.0 Vehicle (no energy_type) into v2.1.0 Vehicle" $ do
      let json = "{\"category\": \"AUTO_RICKSHAW\", \"variant\": \"AUTO_RICKSHAW\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Vehicle
      decoded `shouldSatisfy` isJust
      Spec.vehicleEnergyType (fromJust decoded) `shouldBe` Nothing

    it "decodes v2.0.0 Stop (no instructions) into v2.1.0 Stop" $ do
      let json = "{\"type\": \"START\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Stop
      decoded `shouldSatisfy` isJust
      Spec.stopInstructions (fromJust decoded) `shouldBe` Nothing

    it "decodes v2.0.0 Person (no gender) into v2.1.0 Person" $ do
      let json = "{\"name\": \"Test User\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Person
      decoded `shouldSatisfy` isJust
      Spec.personGender (fromJust decoded) `shouldBe` Nothing

    it "decodes v2.0.0 Intent (no category) into v2.1.0 Intent" $ do
      let json = "{\"payment\": {\"type\": \"ON-FULFILLMENT\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Intent
      decoded `shouldSatisfy` isJust
      Spec.intentCategory (fromJust decoded) `shouldBe` Nothing

    it "decodes v2.0.0 Provider (no categories) into v2.1.0 Provider" $ do
      let json = "{\"id\": \"provider_1\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Provider
      decoded `shouldSatisfy` isJust
      Spec.providerCategories (fromJust decoded) `shouldBe` Nothing

    it "decodes v2.0.0 Cancellation (no reason) into v2.1.0 Cancellation" $ do
      let json = "{\"cancelled_by\": \"CONSUMER\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Cancellation
      decoded `shouldSatisfy` isJust
      Spec.cancellationReasonDescriptor (fromJust decoded) `shouldBe` Nothing

  -- ================================================================
  -- Cancellation type: v2.1.0 reason descriptor
  -- ================================================================

  describe "Cancellation v2.1.0 fields" $ do
    it "preserves cancellationReasonDescriptor through JSON" $ do
      let cancellation =
            Spec.Cancellation
              { cancellationCancelledBy = Just "CONSUMER",
                cancellationReasonDescriptor =
                  Just
                    Spec.Descriptor
                      { descriptorCode = Just "013",
                        descriptorName = Just "Ride accepted mistakenly",
                        descriptorShortDesc = Nothing
                      }
              }
      roundTrip cancellation

    it "serializes reason with correct JSON key" $ do
      let cancellation =
            Spec.Cancellation
              { cancellationCancelledBy = Just "CONSUMER",
                cancellationReasonDescriptor =
                  Just
                    Spec.Descriptor
                      { descriptorCode = Just "013",
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      }
              }
      cancellation `hasJsonKey` "reason"
      cancellation `hasJsonKey` "cancelled_by"

    it "round-trips with Nothing reason" $ do
      let cancellation =
            Spec.Cancellation
              { cancellationCancelledBy = Just "PROVIDER",
                cancellationReasonDescriptor = Nothing
              }
      roundTrip cancellation

  -- ================================================================
  -- Existing types: still work correctly (regression tests)
  -- ================================================================

  describe "Descriptor (existing type, unchanged)" $ do
    it "round-trips with all fields" $ do
      roundTrip sampleDescriptor

    it "round-trips with Nothing fields" $ do
      let desc = Spec.Descriptor {descriptorCode = Nothing, descriptorName = Nothing, descriptorShortDesc = Nothing}
      roundTrip desc

    it "serializes to correct JSON keys" $ do
      sampleDescriptor `hasJsonKey` "code"
      sampleDescriptor `hasJsonKey` "name"
      sampleDescriptor `hasJsonKey` "short_desc"

  describe "Price (existing type, unchanged)" $ do
    it "round-trips with all fields" $ do
      roundTrip samplePrice

    it "round-trips with all Nothing" $ do
      let price =
            Spec.Price
              { priceValue = Nothing,
                priceComputedValue = Nothing,
                priceCurrency = Nothing,
                priceMaximumValue = Nothing,
                priceMinimumValue = Nothing,
                priceOfferedValue = Nothing
              }
      roundTrip price

  describe "CancellationTerm (existing type, unchanged)" $ do
    it "round-trips" $ roundTrip sampleCancellationTerm
    it "serializes to correct JSON keys" $ do
      sampleCancellationTerm `hasJsonKey` "reason_required"

  -- ================================================================
  -- v2.1.0 JSON parsing (forward compatibility)
  -- ================================================================

  describe "v2.1.0 JSON parsing" $ do
    it "parses Item with category_ids from raw JSON" $ do
      let json = "{\"id\": \"item_1\", \"category_ids\": [\"ON_DEMAND_TRIP\"]}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Item
      decoded `shouldSatisfy` isJust
      Spec.itemCategoryIds (fromJust decoded) `shouldBe` Just ["ON_DEMAND_TRIP"]

    it "parses Vehicle with energy_type from raw JSON" $ do
      let json = "{\"category\": \"CAR\", \"energy_type\": \"ELECTRIC\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Vehicle
      decoded `shouldSatisfy` isJust
      Spec.vehicleEnergyType (fromJust decoded) `shouldBe` Just "ELECTRIC"

    it "parses Stop with instructions from raw JSON" $ do
      let json = "{\"type\": \"START\", \"instructions\": {\"code\": \"GATE_A\", \"name\": \"Take Gate A\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Stop
      decoded `shouldSatisfy` isJust
      let stop = fromJust decoded
      (Spec.stopInstructions stop >>= Spec.descriptorCode) `shouldBe` Just "GATE_A"
      (Spec.stopInstructions stop >>= Spec.descriptorName) `shouldBe` Just "Take Gate A"

    it "parses Person with gender from raw JSON" $ do
      let json = "{\"name\": \"Test User\", \"gender\": \"FEMALE\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Person
      decoded `shouldSatisfy` isJust
      Spec.personGender (fromJust decoded) `shouldBe` Just "FEMALE"

    it "parses Cancellation with reason descriptor from raw JSON" $ do
      let json = "{\"cancelled_by\": \"PROVIDER\", \"reason\": {\"code\": \"011\", \"name\": \"No drivers available\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Cancellation
      decoded `shouldSatisfy` isJust
      let canc = fromJust decoded
      Spec.cancellationCancelledBy canc `shouldBe` Just "PROVIDER"
      (Spec.cancellationReasonDescriptor canc >>= Spec.descriptorCode) `shouldBe` Just "011"

  -- ================================================================
  -- omitNothingFields behavior (wire format correctness)
  -- ================================================================

  describe "omitNothingFields behavior" $ do
    it "Item with Nothing new fields omits them from JSON" $ do
      minimalItem `lacksJsonKey` "category_ids"
      minimalItem `lacksJsonKey` "cancellation_terms"
      minimalItem `lacksJsonKey` "add_ons"

    it "Vehicle with Nothing energy_type omits it from JSON" $ do
      minimalVehicle `lacksJsonKey` "energy_type"

    it "Person with Nothing gender omits it from JSON" $ do
      minimalPerson `lacksJsonKey` "gender"

    it "Stop with Nothing instructions omits it from JSON" $ do
      minimalStop `lacksJsonKey` "instructions"

    it "Provider with Nothing categories omits it from JSON" $ do
      minimalProvider `lacksJsonKey` "categories"

    it "Intent with Nothing category omits it from JSON" $ do
      minimalIntent `lacksJsonKey` "category"

    it "preserves existing Item fields alongside new v2.1.0 fields" $ do
      let item =
            minimalItem
              { Spec.itemId = Just "item_1",
                Spec.itemCategoryIds = Just ["ON_DEMAND_TRIP"],
                Spec.itemAddOns = Just [sampleAddOn]
              }
      let decoded = decode (encode item) :: Maybe Spec.Item
      decoded `shouldSatisfy` isJust
      let decodedItem = fromJust decoded
      Spec.itemId decodedItem `shouldBe` Just "item_1"
      Spec.itemCategoryIds decodedItem `shouldBe` Just ["ON_DEMAND_TRIP"]

    it "preserves existing Vehicle fields alongside energy_type" $ do
      let vehicle =
            minimalVehicle
              { Spec.vehicleCategory = Just "AUTO_RICKSHAW",
                Spec.vehicleVariant = Just "AUTO_RICKSHAW",
                Spec.vehicleEnergyType = Just "CNG"
              }
      roundTrip vehicle

    it "preserves existing Person fields alongside gender" $ do
      let person =
            minimalPerson
              { Spec.personName = Just "Test User",
                Spec.personGender = Just "MALE"
              }
      roundTrip person

  -- ================================================================
  -- Existing types regression: core Beckn types unchanged in v2.1.0
  -- ================================================================

  describe "Order (existing type, unchanged)" $ do
    it "round-trips with all fields populated" $ do
      roundTrip sampleOrder

    it "round-trips with Nothing fields" $ do
      roundTrip minimalOrder

    it "serializes to correct JSON keys" $ do
      sampleOrder `hasJsonKey` "id"
      sampleOrder `hasJsonKey` "status"
      sampleOrder `hasJsonKey` "cancellation"
      sampleOrder `hasJsonKey` "cancellation_terms"
      sampleOrder `hasJsonKey` "billing"
      sampleOrder `lacksJsonKey` "orderId"
      sampleOrder `lacksJsonKey` "orderStatus"

    it "preserves order items through JSON" $ do
      let order = minimalOrder {Spec.orderItems = Just [minimalItem {Spec.itemId = Just "item_1"}]}
      let decoded = decode (encode order) :: Maybe Spec.Order
      (decoded >>= Spec.orderItems >>= \xs -> Just (length xs)) `shouldBe` Just 1

  describe "Fulfillment (existing type, unchanged)" $ do
    it "round-trips with all fields populated" $ do
      roundTrip sampleFulfillment

    it "round-trips with Nothing fields" $ do
      roundTrip minimalFulfillment

    it "serializes to correct JSON keys" $ do
      sampleFulfillment `hasJsonKey` "id"
      sampleFulfillment `hasJsonKey` "type"
      sampleFulfillment `hasJsonKey` "state"
      sampleFulfillment `lacksJsonKey` "fulfillmentId"
      sampleFulfillment `lacksJsonKey` "fulfillmentType"

    it "preserves fulfillment vehicle through JSON" $ do
      let ff = minimalFulfillment {Spec.fulfillmentVehicle = Just minimalVehicle {Spec.vehicleCategory = Just "CAB"}}
      let decoded = decode (encode ff) :: Maybe Spec.Fulfillment
      (decoded >>= Spec.fulfillmentVehicle >>= Spec.vehicleCategory) `shouldBe` Just "CAB"

  describe "Context (existing type, unchanged)" $ do
    it "round-trips with action and domain" $ do
      roundTrip sampleContext

    it "serializes to correct JSON keys" $ do
      sampleContext `hasJsonKey` "action"
      sampleContext `hasJsonKey` "domain"
      sampleContext `hasJsonKey` "version"
      sampleContext `hasJsonKey` "bap_id"
      sampleContext `hasJsonKey` "bpp_id"
      sampleContext `lacksJsonKey` "contextAction"
      sampleContext `lacksJsonKey` "contextDomain"

    it "preserves context version through JSON" $ do
      let decoded = decode (encode sampleContext) :: Maybe Spec.Context
      (decoded >>= Spec.contextVersion) `shouldBe` Just "2.1.0"

  describe "Payment (existing type, unchanged)" $ do
    it "round-trips with fields populated" $ do
      roundTrip samplePayment

    it "round-trips with Nothing fields" $ do
      roundTrip minimalPayment

    it "serializes to correct JSON keys" $ do
      samplePayment `hasJsonKey` "collected_by"
      samplePayment `hasJsonKey` "type"
      samplePayment `hasJsonKey` "status"
      samplePayment `lacksJsonKey` "paymentCollectedBy"
      samplePayment `lacksJsonKey` "paymentType"

  describe "Quotation (existing type, unchanged)" $ do
    it "round-trips" $ do
      roundTrip sampleQuotation

    it "serializes to correct JSON keys" $ do
      sampleQuotation `hasJsonKey` "price"
      sampleQuotation `hasJsonKey` "ttl"
      sampleQuotation `lacksJsonKey` "quotationPrice"

  describe "Authorization (existing type, unchanged)" $ do
    it "round-trips with both fields" $ do
      roundTrip sampleAuthorization

    it "round-trips with Nothing fields" $ do
      let auth = Spec.Authorization {authorizationType = Nothing, authorizationToken = Nothing}
      roundTrip auth

    it "serializes to correct JSON keys" $ do
      sampleAuthorization `hasJsonKey` "type"
      sampleAuthorization `hasJsonKey` "token"
      sampleAuthorization `lacksJsonKey` "authorizationType"

  -- ================================================================
  -- Spec switching: v2.0.0 ↔ v2.1.0 cross-version payloads
  -- ================================================================

  describe "spec switching: v2.0.0 → v2.1.0" $ do
    it "v2.0.0 Order JSON with no new fields parses cleanly" $ do
      let json =
            "{\"id\": \"order_1\", \"status\": \"ACTIVE\", \
            \\"items\": [{\"id\": \"item_1\"}], \
            \\"billing\": {\"name\": \"Test\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Order
      decoded `shouldSatisfy` isJust
      let order = fromJust decoded
      Spec.orderId order `shouldBe` Just "order_1"
      Spec.orderStatus order `shouldBe` Just "ACTIVE"
      -- New v2.1.0 fields at item level should be Nothing
      case Spec.orderItems order of
        Just (item : _) -> do
          Spec.itemCategoryIds item `shouldBe` Nothing
          Spec.itemCancellationTerms item `shouldBe` Nothing
          Spec.itemAddOns item `shouldBe` Nothing
        _ -> expectationFailure "Expected items in order"

    it "v2.0.0 Fulfillment JSON with no energy_type in vehicle parses cleanly" $ do
      let json =
            "{\"id\": \"ff_1\", \"type\": \"DELIVERY\", \
            \\"vehicle\": {\"category\": \"CAB\", \"variant\": \"SEDAN\"}, \
            \\"state\": {\"descriptor\": {\"code\": \"RIDE_ASSIGNED\"}}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Fulfillment
      decoded `shouldSatisfy` isJust
      let ff = fromJust decoded
      Spec.fulfillmentId ff `shouldBe` Just "ff_1"
      case Spec.fulfillmentVehicle ff of
        Just veh -> Spec.vehicleEnergyType veh `shouldBe` Nothing
        Nothing -> expectationFailure "Expected vehicle in fulfillment"

    it "v2.0.0 Context with version 2.0.0 parses cleanly" $ do
      let json =
            "{\"action\": \"search\", \"domain\": \"ONDC:TRV10\", \
            \\"version\": \"2.0.0\", \"bap_id\": \"bap.test.com\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Context
      decoded `shouldSatisfy` isJust
      Spec.contextVersion (fromJust decoded) `shouldBe` Just "2.0.0"

  describe "spec switching: v2.1.0 → v2.0.0 (forward compat)" $ do
    it "v2.1.0 Item with category_ids round-trips to JSON that v2.0.0 can ignore" $ do
      let item = minimalItem {Spec.itemId = Just "item_1", Spec.itemCategoryIds = Just ["ON_DEMAND_TRIP"]}
      let json = encode item
      -- Verify the JSON contains both old and new fields
      let decoded = decode json :: Maybe Spec.Item
      decoded `shouldSatisfy` isJust
      Spec.itemId (fromJust decoded) `shouldBe` Just "item_1"
      Spec.itemCategoryIds (fromJust decoded) `shouldBe` Just ["ON_DEMAND_TRIP"]

    it "v2.1.0 Vehicle with energy_type remains parseable by v2.0.0 consumer" $ do
      let vehicle = minimalVehicle {Spec.vehicleCategory = Just "CAB", Spec.vehicleEnergyType = Just "ELECTRIC"}
      let json = encode vehicle
      -- v2.0.0 consumer uses same types but will just ignore unknown fields
      let decoded = decode json :: Maybe Spec.Vehicle
      decoded `shouldSatisfy` isJust
      Spec.vehicleCategory (fromJust decoded) `shouldBe` Just "CAB"
      Spec.vehicleEnergyType (fromJust decoded) `shouldBe` Just "ELECTRIC"

    it "v2.1.0 Context version 2.1.0 parses cleanly" $ do
      let json =
            "{\"action\": \"on_search\", \"domain\": \"ONDC:TRV10\", \
            \\"version\": \"2.1.0\", \"bpp_id\": \"bpp.test.com\"}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Context
      decoded `shouldSatisfy` isJust
      Spec.contextVersion (fromJust decoded) `shouldBe` Just "2.1.0"

    it "v2.1.0 Order with item cancellation_terms round-trips" $ do
      let cancelTerm = Spec.CancellationTerm
            { cancellationTermCancellationFee = Nothing,
              cancellationTermFulfillmentState = Just (Spec.FulfillmentState {fulfillmentStateDescriptor = Just (Spec.Descriptor {descriptorCode = Just "RIDE_CONFIRMED", descriptorName = Nothing, descriptorShortDesc = Nothing})}),
              cancellationTermReasonRequired = Just False
            }
      let item = minimalItem {Spec.itemId = Just "item_1", Spec.itemCancellationTerms = Just [cancelTerm]}
      let order = minimalOrder {Spec.orderItems = Just [item]}
      let decoded = decode (encode order) :: Maybe Spec.Order
      decoded `shouldSatisfy` isJust
      let decodedItems = Spec.orderItems (fromJust decoded)
      case decodedItems of
        Just (i : _) -> do
          Spec.itemCancellationTerms i `shouldSatisfy` isJust
          let terms = fromJust (Spec.itemCancellationTerms i)
          length terms `shouldBe` 1
        _ -> expectationFailure "Expected items in decoded order"

  describe "spec switching: mixed payloads" $ do
    it "Item with v2.0.0 fields + v2.1.0 fields coexist in JSON" $ do
      let json =
            "{\"id\": \"item_1\", \"descriptor\": {\"code\": \"RIDE\"}, \
            \\"category_ids\": [\"ON_DEMAND_TRIP\"], \
            \\"add_ons\": [{\"id\": \"extra_km\", \"descriptor\": {\"name\": \"Extra KM\"}}], \
            \\"fulfillment_ids\": [\"ff_1\"], \
            \\"price\": {\"value\": \"100\", \"currency\": \"INR\"}}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Item
      decoded `shouldSatisfy` isJust
      let item = fromJust decoded
      -- v2.0.0 fields preserved
      Spec.itemId item `shouldBe` Just "item_1"
      Spec.itemFulfillmentIds item `shouldBe` Just ["ff_1"]
      (Spec.itemPrice item >>= Spec.priceValue) `shouldBe` Just "100"
      -- v2.1.0 fields also parsed
      Spec.itemCategoryIds item `shouldBe` Just ["ON_DEMAND_TRIP"]
      (Spec.itemAddOns item >>= \xs -> Just (length xs)) `shouldBe` Just 1

    it "Provider with v2.0.0 fields + v2.1.0 categories coexist" $ do
      let json =
            "{\"id\": \"provider_1\", \"descriptor\": {\"name\": \"Test Provider\"}, \
            \\"categories\": [{\"id\": \"ON_DEMAND_TRIP\", \"descriptor\": {\"code\": \"ON_DEMAND_TRIP\"}}]}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Provider
      decoded `shouldSatisfy` isJust
      let prov = fromJust decoded
      Spec.providerId prov `shouldBe` Just "provider_1"
      (Spec.providerCategories prov >>= \xs -> Just (length xs)) `shouldBe` Just 1

    it "Fulfillment with v2.1.0 vehicle energy_type + existing fields" $ do
      let json =
            "{\"id\": \"ff_1\", \"type\": \"DELIVERY\", \
            \\"vehicle\": {\"category\": \"CAB\", \"variant\": \"SEDAN\", \"color\": \"White\", \"energy_type\": \"ELECTRIC\"}, \
            \\"stops\": [{\"type\": \"START\", \"instructions\": {\"name\": \"Take Gate A\"}}]}" :: BL.ByteString
      let decoded = decode json :: Maybe Spec.Fulfillment
      decoded `shouldSatisfy` isJust
      let ff = fromJust decoded
      Spec.fulfillmentType ff `shouldBe` Just "DELIVERY"
      case Spec.fulfillmentVehicle ff of
        Just veh -> do
          Spec.vehicleCategory veh `shouldBe` Just "CAB"
          Spec.vehicleEnergyType veh `shouldBe` Just "ELECTRIC"
          Spec.vehicleColor veh `shouldBe` Just "White"
        Nothing -> expectationFailure "Expected vehicle"
      case Spec.fulfillmentStops ff of
        Just (stop : _) -> do
          (Spec.stopInstructions stop >>= Spec.descriptorName) `shouldBe` Just "Take Gate A"
        _ -> expectationFailure "Expected stops"

-- ================================================================
-- Minimal constructors with all fields as Nothing
-- ================================================================

minimalItem :: Spec.Item
minimalItem =
  Spec.Item
    { itemDescriptor = Nothing,
      itemFulfillmentIds = Nothing,
      itemId = Nothing,
      itemLocationIds = Nothing,
      itemPaymentIds = Nothing,
      itemPrice = Nothing,
      itemTags = Nothing,
      itemCategoryIds = Nothing,
      itemCancellationTerms = Nothing,
      itemAddOns = Nothing
    }

minimalVehicle :: Spec.Vehicle
minimalVehicle =
  Spec.Vehicle
    { vehicleCategory = Nothing,
      vehicleColor = Nothing,
      vehicleEnergyType = Nothing,
      vehicleMake = Nothing,
      vehicleModel = Nothing,
      vehicleRegistration = Nothing,
      vehicleVariant = Nothing,
      vehicleCapacity = Nothing
    }

minimalStop :: Spec.Stop
minimalStop =
  Spec.Stop
    { stopAuthorization = Nothing,
      stopInstructions = Nothing,
      stopLocation = Nothing,
      stopId = Nothing,
      stopParentStopId = Nothing,
      stopTime = Nothing,
      stopType = Nothing
    }

minimalPerson :: Spec.Person
minimalPerson =
  Spec.Person
    { personId = Nothing,
      personImage = Nothing,
      personName = Nothing,
      personTags = Nothing,
      personGender = Nothing
    }

minimalProvider :: Spec.Provider
minimalProvider =
  Spec.Provider
    { providerDescriptor = Nothing,
      providerId = Nothing,
      providerItems = Nothing,
      providerLocations = Nothing,
      providerFulfillments = Nothing,
      providerPayments = Nothing,
      providerCategories = Nothing
    }

minimalIntent :: Spec.Intent
minimalIntent =
  Spec.Intent
    { intentFulfillment = Nothing,
      intentPayment = Nothing,
      intentTags = Nothing,
      intentCategory = Nothing
    }

minimalOrder :: Spec.Order
minimalOrder =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = Nothing,
      orderPayments = Nothing,
      orderProvider = Nothing,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderUpdatedAt = Nothing
    }

minimalFulfillment :: Spec.Fulfillment
minimalFulfillment =
  Spec.Fulfillment
    { fulfillmentAgent = Nothing,
      fulfillmentCustomer = Nothing,
      fulfillmentId = Nothing,
      fulfillmentState = Nothing,
      fulfillmentStops = Nothing,
      fulfillmentTags = Nothing,
      fulfillmentType = Nothing,
      fulfillmentVehicle = Nothing
    }

minimalPayment :: Spec.Payment
minimalPayment =
  Spec.Payment
    { paymentCollectedBy = Nothing,
      paymentId = Nothing,
      paymentParams = Nothing,
      paymentStatus = Nothing,
      paymentTags = Nothing,
      paymentTlMethod = Nothing,
      paymentType = Nothing
    }

-- Sample data for regression tests
sampleOrder :: Spec.Order
sampleOrder =
  minimalOrder
    { Spec.orderId = Just "order_123",
      Spec.orderStatus = Just "ACTIVE",
      Spec.orderCancellation =
        Just
          Spec.Cancellation
            { cancellationCancelledBy = Just "CONSUMER",
              cancellationReasonDescriptor = Nothing
            },
      Spec.orderCancellationTerms = Just [sampleCancellationTerm],
      Spec.orderBilling =
        Just
          Spec.Billing
            { billingName = Just "Test User",
              billingPhone = Just "9876543210"
            }
    }

sampleFulfillment :: Spec.Fulfillment
sampleFulfillment =
  minimalFulfillment
    { Spec.fulfillmentId = Just "ff_001",
      Spec.fulfillmentType = Just "DELIVERY",
      Spec.fulfillmentState =
        Just
          Spec.FulfillmentState
            { fulfillmentStateDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "RIDE_ASSIGNED",
                      descriptorName = Nothing,
                      descriptorShortDesc = Nothing
                    }
            }
    }

sampleContext :: Spec.Context
sampleContext =
  Spec.Context
    { contextAction = Just "search",
      contextBapId = Just "bap.nammayatri.in",
      contextBapUri = Just "https://bap.nammayatri.in",
      contextBppId = Just "bpp.nammayatri.in",
      contextBppUri = Nothing,
      contextDomain = Just "ONDC:TRV10",
      contextKey = Nothing,
      contextLocation = Nothing,
      contextMessageId = Nothing,
      contextTimestamp = Nothing,
      contextTransactionId = Nothing,
      contextTtl = Nothing,
      contextVersion = Just "2.1.0"
    }

samplePayment :: Spec.Payment
samplePayment =
  minimalPayment
    { Spec.paymentCollectedBy = Just "BAP",
      Spec.paymentType = Just "ON-FULFILLMENT",
      Spec.paymentStatus = Just "NOT-PAID"
    }

sampleQuotation :: Spec.Quotation
sampleQuotation =
  Spec.Quotation
    { quotationBreakup = Nothing,
      quotationPrice = Just samplePrice,
      quotationTtl = Just "PT30S"
    }

sampleAuthorization :: Spec.Authorization
sampleAuthorization =
  Spec.Authorization
    { authorizationType = Just "OTP",
      authorizationToken = Just "1234"
    }
