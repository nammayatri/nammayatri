module BecknV2.OnDemand.EnumsSpec (spec) where

import BecknV2.OnDemand.Enums
import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import Kernel.Prelude
import Test.Hspec

-- | Helper: round-trip via JSON
roundTrip :: (Eq a, Show a, A.ToJSON a, A.FromJSON a) => a -> Expectation
roundTrip val = decode (encode val) `shouldBe` Just val

-- | Helper: check a value serializes to a specific JSON string
serializesTo :: (Show a, A.ToJSON a) => a -> Text -> Expectation
serializesTo val expected = A.toJSON val `shouldBe` A.String expected

-- | Helper: check show output against a String literal (avoids type-defaults warning)
showsAs :: Show a => a -> String -> Expectation
showsAs val expected = show val `shouldBe` expected

spec :: Spec
spec = describe "BecknV2.OnDemand.Enums" $ do
  -- ================================================================
  -- FulfillmentState (new: RIDE_CONFIRMED)
  -- ================================================================

  describe "FulfillmentState" $ do
    it "RIDE_CONFIRMED round-trips through JSON" $ do
      roundTrip RIDE_CONFIRMED

    it "RIDE_CONFIRMED serializes to \"RIDE_CONFIRMED\"" $ do
      RIDE_CONFIRMED `serializesTo` "RIDE_CONFIRMED"

    it "RIDE_ASSIGNED still round-trips (unchanged)" $ do
      roundTrip RIDE_ASSIGNED

    it "SCHEDULED_RIDE_ASSIGNED round-trips" $ do
      roundTrip SCHEDULED_RIDE_ASSIGNED

    it "all standard states round-trip" $ do
      mapM_
        roundTrip
        [ RIDE_CANCELLED,
          RIDE_ENDED,
          RIDE_STARTED,
          RIDE_ASSIGNED,
          RIDE_CONFIRMED,
          RIDE_ENROUTE_PICKUP,
          RIDE_ARRIVED_PICKUP,
          SCHEDULED_RIDE_ASSIGNED,
          DRIVER_REACHED_DESTINATION
        ]

  -- ================================================================
  -- EnergyType (new v2.1.0 enum)
  -- ================================================================

  describe "EnergyType" $ do
    it "ELECTRIC round-trips" $ do
      roundTrip ELECTRIC

    it "CNG round-trips" $ do
      roundTrip CNG

    it "all energy types round-trip" $ do
      mapM_ roundTrip [ELECTRIC, PETROL, DIESEL, HYDROGEN, BIOFUELS, CNG, LPG]

    it "ELECTRIC serializes to \"ELECTRIC\"" $ do
      ELECTRIC `serializesTo` "ELECTRIC"

    it "LPG serializes to \"LPG\"" $ do
      LPG `serializesTo` "LPG"

  -- ================================================================
  -- CancellationReasonId (new: TECHNICAL_CANCELLATION, BOOKED_BY_MISTAKE)
  -- ================================================================

  describe "CancellationReasonId" $ do
    it "TECHNICAL_CANCELLATION shows as \"000\"" $ do
      TECHNICAL_CANCELLATION `showsAs` "000"

    it "BOOKED_BY_MISTAKE shows as \"005\"" $ do
      BOOKED_BY_MISTAKE `showsAs` "005"

    it "existing codes unchanged" $ do
      DRIVER_NOT_MOVING `showsAs` "001"
      DRIVER_NOT_REACHABLE `showsAs` "002"
      DRIVER_ASKED_TO_CANCEL `showsAs` "003"
      INCORRECT_PICKUP_LOCATION `showsAs` "004"

  -- ================================================================
  -- CancellationReasonCode (new: UNABLE_TO_CONTACT_RIDER)
  -- ================================================================

  describe "CancellationReasonCode" $ do
    it "UNABLE_TO_CONTACT_RIDER shows as \"014\"" $ do
      UNABLE_TO_CONTACT_RIDER `showsAs` "014"

    it "existing codes unchanged" $ do
      NO_DRIVERS_AVAILABLE `showsAs` "011"
      COULD_NOT_FIND_CUSTOMER `showsAs` "012"
      RIDE_ACCEPTED_MISTAKENLY `showsAs` "013"

  -- ================================================================
  -- QuoteBreakupTitle (new: BUYER_ADDITIONAL_AMOUNT)
  -- ================================================================

  describe "QuoteBreakupTitle" $ do
    it "BUYER_ADDITIONAL_AMOUNT round-trips" $ do
      roundTrip BUYER_ADDITIONAL_AMOUNT

    it "BUYER_ADDITIONAL_AMOUNT serializes to \"BUYER_ADDITIONAL_AMOUNT\"" $ do
      BUYER_ADDITIONAL_AMOUNT `serializesTo` "BUYER_ADDITIONAL_AMOUNT"

    it "existing titles still round-trip" $ do
      mapM_
        roundTrip
        [ BASE_FARE,
          DISTANCE_FARE,
          TOLL_CHARGES,
          CANCELLATION_CHARGES,
          DRIVER_SELECTED_FARE,
          CUSTOMER_SELECTED_FARE
        ]

  -- ================================================================
  -- PaymentStatus (custom serialization with hyphen)
  -- ================================================================

  describe "PaymentStatus" $ do
    it "NOT_PAID serializes to \"NOT-PAID\"" $ do
      NOT_PAID `serializesTo` "NOT-PAID"

    it "PAID serializes to \"PAID\"" $ do
      PAID `serializesTo` "PAID"

    it "NOT_PAID round-trips" $ do
      roundTrip NOT_PAID

  -- ================================================================
  -- OrderStatus
  -- ================================================================

  describe "OrderStatus" $ do
    it "SOFT_UPDATE round-trips" $ do
      roundTrip SOFT_UPDATE

    it "CONFIRM_UPDATE round-trips" $ do
      roundTrip CONFIRM_UPDATE

    it "all order statuses round-trip" $ do
      mapM_
        roundTrip
        [SOFT_CANCEL, CONFIRM_CANCEL, ACTIVE, COMPLETE, CANCELLED, SOFT_UPDATE, CONFIRM_UPDATE]

  -- ================================================================
  -- Existing enums: regression tests (unchanged in v2.1.0)
  -- ================================================================

  describe "VehicleCategory (existing, unchanged)" $ do
    it "all vehicle categories round-trip" $ do
      mapM_
        roundTrip
        [AUTO_RICKSHAW, CAB, MOTORCYCLE, METRO, SUBWAY, BUS, AMBULANCE, TWO_WHEELER, TRUCK, BOAT, TOTO]

    it "AUTO_RICKSHAW serializes to \"AUTO_RICKSHAW\"" $ do
      AUTO_RICKSHAW `serializesTo` "AUTO_RICKSHAW"

    it "CAB serializes to \"CAB\"" $ do
      CAB `serializesTo` "CAB"

  describe "FulfillmentType (existing, unchanged)" $ do
    it "all fulfillment types round-trip" $ do
      mapM_
        roundTrip
        [DELIVERY, RIDE_OTP, RENTAL, INTER_CITY, AMBULANCE_FLOW, METER_RIDE, SCHEDULED_TRIP]

    it "DELIVERY serializes to \"DELIVERY\"" $ do
      DELIVERY `serializesTo` "DELIVERY"

    it "RENTAL serializes to \"RENTAL\"" $ do
      RENTAL `serializesTo` "RENTAL"

    it "SCHEDULED_TRIP serializes to \"SCHEDULED_TRIP\"" $ do
      SCHEDULED_TRIP `serializesTo` "SCHEDULED_TRIP"

  describe "StopType (existing, unchanged)" $ do
    it "all stop types round-trip" $ do
      mapM_ roundTrip [START, END, INTERMEDIATE_STOP]

    it "START serializes to \"START\"" $ do
      START `serializesTo` "START"

    it "INTERMEDIATE_STOP serializes to \"INTERMEDIATE_STOP\"" $ do
      INTERMEDIATE_STOP `serializesTo` "INTERMEDIATE_STOP"

  describe "AuthorizationType (existing, unchanged)" $ do
    it "OTP round-trips" $ do
      roundTrip OTP

    it "QR round-trips" $ do
      roundTrip QR

    it "OTP serializes to \"OTP\"" $ do
      OTP `serializesTo` "OTP"

  describe "PaymentType (existing, unchanged)" $ do
    it "ON_FULFILLMENT serializes to \"ON-FULFILLMENT\"" $ do
      ON_FULFILLMENT `serializesTo` "ON-FULFILLMENT"

    it "PRE_ORDER serializes to \"PRE-ORDER\"" $ do
      PRE_ORDER `serializesTo` "PRE-ORDER"

    it "POST_FULFILLMENT serializes to \"POST-FULFILLMENT\"" $ do
      POST_FULFILLMENT `serializesTo` "POST-FULFILLMENT"

    it "ON_FULFILLMENT round-trips" $ do
      roundTrip ON_FULFILLMENT

    it "PRE_ORDER round-trips" $ do
      roundTrip PRE_ORDER

  describe "PaymentCollectedBy (existing, unchanged)" $ do
    it "BAP round-trips" $ do
      roundTrip BAP

    it "BPP round-trips" $ do
      roundTrip BPP

    it "SELLER round-trips" $ do
      roundTrip SELLER

  describe "CancelReqMessageCancellationReasonId (existing, unchanged)" $ do
    it "CANCELLED_BY_CUSTOMER shows as \"001\"" $ do
      CANCELLED_BY_CUSTOMER `showsAs` "001"

    it "CANCELLED_BY_DRIVER shows as \"002\"" $ do
      CANCELLED_BY_DRIVER `showsAs` "002"

    it "CANCELLED_BY_CUSTOMER round-trips" $ do
      roundTrip CANCELLED_BY_CUSTOMER

    it "CANCELLED_BY_DRIVER round-trips" $ do
      roundTrip CANCELLED_BY_DRIVER
