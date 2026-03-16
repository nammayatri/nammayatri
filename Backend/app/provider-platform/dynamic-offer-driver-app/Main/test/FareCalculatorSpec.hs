{-# LANGUAGE OverloadedStrings #-}

module FareCalculatorSpec (spec) where

import "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant (NightShiftBounds (..))
import Data.Time
import Domain.Types.CancellationFarePolicy as DTCFP
import Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.FareCalculator
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkFareParamsId :: Id FareParameters
mkFareParamsId = Id "test-fare-params-id"

-- A fixed timestamp for reproducible tests (2024-01-15 14:00:00 UTC = 19:30 IST, daytime)
dayTime :: UTCTime
dayTime = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 14 0 0)

-- Standard night shift bounds: 10 PM to 5 AM IST
standardNightShiftBounds :: NightShiftBounds
standardNightShiftBounds =
  NightShiftBounds
    { nightShiftStart = TimeOfDay 22 0 0,
      nightShiftEnd = TimeOfDay 5 0 0
    }

-- Build a minimal progressive FareParameters for testing sums.
-- All optional charges default to Nothing; use with* helpers to set them.
mkFP ::
  HighPrecMoney -> -- baseFare
  HighPrecMoney -> -- deadKmFare
  Maybe HighPrecMoney -> -- extraKmFare
  FareParameters
mkFP bf dkf ekf =
  FareParameters
    { id = mkFareParamsId,
      driverSelectedFare = Nothing,
      customerExtraFee = Nothing,
      serviceCharge = Nothing,
      parkingCharge = Nothing,
      stopCharges = Nothing,
      govtCharges = Nothing,
      baseFare = bf,
      waitingCharge = Nothing,
      rideExtraTimeFare = Nothing,
      nightShiftCharge = Nothing,
      nightShiftRateIfApplies = Nothing,
      fareParametersDetails =
        ProgressiveDetails $
          FParamsProgressiveDetails
            { deadKmFare = dkf,
              extraKmFare = ekf,
              rideDurationFare = Nothing,
              currency = INR
            },
      customerCancellationDues = Nothing,
      tollCharges = Nothing,
      congestionCharge = Nothing,
      petCharges = Nothing,
      driverAllowance = Nothing,
      businessDiscount = Nothing,
      personalDiscount = Nothing,
      priorityCharges = Nothing,
      congestionChargeViaDp = Nothing,
      insuranceCharge = Nothing,
      cardCharge = Nothing,
      luggageCharge = Nothing,
      returnFeeCharge = Nothing,
      boothCharge = Nothing,
      platformFee = Nothing,
      sgst = Nothing,
      cgst = Nothing,
      platformFeeChargesBy = DFP.None,
      currency = INR,
      updatedAt = dayTime,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing,
      conditionalCharges = [],
      shouldApplyBusinessDiscount = False,
      shouldApplyPersonalDiscount = False,
      driverCancellationPenaltyAmount = Nothing,
      paymentProcessingFee = Nothing,
      rideVat = Nothing,
      tollVat = Nothing
    }

-- Field setter helpers to avoid ambiguous record update with DuplicateRecordFields
withServiceCharge :: HighPrecMoney -> FareParameters -> FareParameters
withServiceCharge v fp = fp {DFParams.serviceCharge = Just v}

withWaitingCharge :: HighPrecMoney -> FareParameters -> FareParameters
withWaitingCharge v fp = fp {DFParams.waitingCharge = Just v}

withNightShiftCharge :: HighPrecMoney -> FareParameters -> FareParameters
withNightShiftCharge v fp = fp {DFParams.nightShiftCharge = Just v}

withTollCharges :: HighPrecMoney -> FareParameters -> FareParameters
withTollCharges v fp = fp {DFParams.tollCharges = Just v}

withCongestionCharge :: HighPrecMoney -> FareParameters -> FareParameters
withCongestionCharge v fp = fp {DFParams.congestionCharge = Just v}

withParkingCharge :: HighPrecMoney -> FareParameters -> FareParameters
withParkingCharge v fp = fp {DFParams.parkingCharge = Just v}

withInsuranceCharge :: HighPrecMoney -> FareParameters -> FareParameters
withInsuranceCharge v fp = fp {DFParams.insuranceCharge = Just v}

withRideVat :: HighPrecMoney -> FareParameters -> FareParameters
withRideVat v fp = fp {DFParams.rideVat = Just v}

withTollVat :: HighPrecMoney -> FareParameters -> FareParameters
withTollVat v fp = fp {DFParams.tollVat = Just v}

withCardCharge :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> FareParameters -> FareParameters
withCardCharge onF fixed fp = fp {DFParams.cardCharge = Just $ DFParams.CardCharge onF fixed}

withPaymentProcessingFee :: HighPrecMoney -> FareParameters -> FareParameters
withPaymentProcessingFee v fp = fp {DFParams.paymentProcessingFee = Just v}

withCustomerCancellationDues :: HighPrecMoney -> FareParameters -> FareParameters
withCustomerCancellationDues v fp = fp {DFParams.customerCancellationDues = Just v}

withPetCharges :: HighPrecMoney -> FareParameters -> FareParameters
withPetCharges v fp = fp {DFParams.petCharges = Just v}

withDriverAllowance :: HighPrecMoney -> FareParameters -> FareParameters
withDriverAllowance v fp = fp {DFParams.driverAllowance = Just v}

withStopCharges :: HighPrecMoney -> FareParameters -> FareParameters
withStopCharges v fp = fp {DFParams.stopCharges = Just v}

withPriorityCharges :: HighPrecMoney -> FareParameters -> FareParameters
withPriorityCharges v fp = fp {DFParams.priorityCharges = Just v}

withLuggageCharge :: HighPrecMoney -> FareParameters -> FareParameters
withLuggageCharge v fp = fp {DFParams.luggageCharge = Just v}

withRideExtraTimeFare :: HighPrecMoney -> FareParameters -> FareParameters
withRideExtraTimeFare v fp = fp {DFParams.rideExtraTimeFare = Just v}

withDriverSelectedFare :: HighPrecMoney -> FareParameters -> FareParameters
withDriverSelectedFare v fp = fp {DFParams.driverSelectedFare = Just v}

withCustomerExtraFee :: HighPrecMoney -> FareParameters -> FareParameters
withCustomerExtraFee v fp = fp {DFParams.customerExtraFee = Just v}

withBusinessDiscount :: HighPrecMoney -> FareParameters -> FareParameters
withBusinessDiscount v fp = fp {DFParams.businessDiscount = Just v, DFParams.shouldApplyBusinessDiscount = True}

withPersonalDiscount :: HighPrecMoney -> FareParameters -> FareParameters
withPersonalDiscount v fp = fp {DFParams.personalDiscount = Just v, DFParams.shouldApplyPersonalDiscount = True}

-- Business discount set but not applied
withBusinessDiscountNoApply :: HighPrecMoney -> FareParameters -> FareParameters
withBusinessDiscountNoApply v fp = fp {DFParams.businessDiscount = Just v, DFParams.shouldApplyBusinessDiscount = False}

mkCancellationFarePolicy :: DTCFP.CancellationFarePolicy
mkCancellationFarePolicy =
  DTCFP.CancellationFarePolicy
    { currency = INR,
      description = "Test cancellation policy",
      freeCancellationTimeSeconds = Seconds 300,
      id = Id "test-cancellation-policy",
      maxCancellationCharge = 100,
      maxWaitingTimeAtPickupSeconds = Seconds 300,
      minCancellationCharge = 25,
      perMetreCancellationCharge = 0.005,
      perMinuteCancellationCharge = 2,
      percentageOfRideFareToBeCharged = 0.5,
      createdAt = dayTime,
      updatedAt = dayTime
    }

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "pureFareSum" pureFareSumSpecs
  describe "fareSum" fareSumSpecs
  describe "isNightShift" nightShiftSpecs
  describe "calculateCancellationCharges" cancellationChargeSpecs
  describe "calculateNoShowCharges" noShowChargeSpecs
  describe "calculateBusinessDiscount" businessDiscountSpecs
  describe "Integration: fare scenarios" integrationSpecs

-- ---------------------------------------------------------------------------
-- pureFareSum
-- ---------------------------------------------------------------------------

pureFareSumSpecs :: Spec
pureFareSumSpecs = do
  it "calculates base fare only for minimal params" $ do
    pureFareSum (mkFP 100 0 Nothing) Nothing `shouldBe` 100

  it "includes deadKmFare in total" $ do
    -- baseFare(50) + deadKmFare(30) = 80
    pureFareSum (mkFP 50 30 Nothing) Nothing `shouldBe` 80

  it "includes extraKmFare in total" $ do
    -- baseFare(50) + deadKmFare(30) + extraKmFare(45) = 125
    pureFareSum (mkFP 50 30 (Just 45)) Nothing `shouldBe` 125

  it "includes service charge" $ do
    pureFareSum (withServiceCharge 15 $ mkFP 100 0 Nothing) Nothing `shouldBe` 115

  it "includes waiting charge" $ do
    pureFareSum (withWaitingCharge 20 $ mkFP 100 0 Nothing) Nothing `shouldBe` 120

  it "includes night shift charge" $ do
    pureFareSum (withNightShiftCharge 30 $ mkFP 100 0 Nothing) Nothing `shouldBe` 130

  it "includes toll charges" $ do
    pureFareSum (withTollCharges 50 $ mkFP 100 0 Nothing) Nothing `shouldBe` 150

  it "includes congestion charge" $ do
    pureFareSum (withCongestionCharge 25 $ mkFP 100 0 Nothing) Nothing `shouldBe` 125

  it "includes parking charge" $ do
    pureFareSum (withParkingCharge 10 $ mkFP 100 0 Nothing) Nothing `shouldBe` 110

  it "includes insurance charge" $ do
    pureFareSum (withInsuranceCharge 5 $ mkFP 100 0 Nothing) Nothing `shouldBe` 105

  it "includes ride VAT (V2)" $ do
    pureFareSum (withRideVat 14 $ mkFP 100 0 Nothing) Nothing `shouldBe` 114

  it "includes toll VAT (V2)" $ do
    let fp = withTollVat 12.5 . withTollCharges 50 $ mkFP 100 0 Nothing
    pureFareSum fp Nothing `shouldBe` 162.5

  it "includes card charge (onFare + fixed)" $ do
    let fp = withCardCharge (Just 8) (Just 5) $ mkFP 100 0 Nothing
    pureFareSum fp Nothing `shouldBe` 113

  it "includes payment processing fee" $ do
    pureFareSum (withPaymentProcessingFee 3 $ mkFP 100 0 Nothing) Nothing `shouldBe` 103

  it "includes customer cancellation dues" $ do
    pureFareSum (withCustomerCancellationDues 25 $ mkFP 100 0 Nothing) Nothing `shouldBe` 125

  it "includes all charges combined" $ do
    let fp =
          withServiceCharge 15
            . withWaitingCharge 10
            . withNightShiftCharge 25
            . withTollCharges 40
            . withCongestionCharge 20
            . withParkingCharge 5
            . withInsuranceCharge 3
            . withPetCharges 2
            . withDriverAllowance 10
            . withStopCharges 8
            . withPriorityCharges 12
            . withLuggageCharge 7
            . withRideVat 14
            . withTollVat 10
            . withPaymentProcessingFee 2
            $ mkFP 100 20 (Just 30)
    -- baseFare(100) + serviceCharge(15) + waitingCharge(10)
    -- + nightShiftCharge(25) + congestionCharge(20)
    -- + petCharges(2) + driverAllowance(10) + stopCharges(8) + priorityCharges(12)
    -- + extraKmFare(30) + deadKmFare(20)
    -- + tollCharges(40) + parkingCharge(5)
    -- + insuranceCharge(3) + luggageCharge(7)
    -- + paymentProcessingFee(2) + rideVat(14) + tollVat(10) = 333
    pureFareSum fp Nothing `shouldBe` 333

  it "does not include business/personal discounts (those are in fareSum)" $ do
    let fp = withBusinessDiscount 20 . withPersonalDiscount 10 $ mkFP 200 0 Nothing
    -- pureFareSum should still be 200 (discounts applied only in fareSum)
    pureFareSum fp Nothing `shouldBe` 200

-- ---------------------------------------------------------------------------
-- fareSum
-- ---------------------------------------------------------------------------

fareSumSpecs :: Spec
fareSumSpecs = do
  it "equals pureFareSum when no extras or discounts" $ do
    let fp = mkFP 100 20 (Just 30)
    fareSum fp Nothing `shouldBe` pureFareSum fp Nothing

  it "includes driverSelectedFare" $ do
    fareSum (withDriverSelectedFare 15 $ mkFP 100 0 Nothing) Nothing `shouldBe` 115

  it "includes customerExtraFee" $ do
    fareSum (withCustomerExtraFee 20 $ mkFP 100 0 Nothing) Nothing `shouldBe` 120

  it "subtracts business discount when flag is set" $ do
    -- 200 - 40 = 160
    fareSum (withBusinessDiscount 40 $ mkFP 200 0 Nothing) Nothing `shouldBe` 160

  it "does not subtract business discount when flag is False" $ do
    fareSum (withBusinessDiscountNoApply 40 $ mkFP 200 0 Nothing) Nothing `shouldBe` 200

  it "subtracts personal discount when flag is set" $ do
    fareSum (withPersonalDiscount 30 $ mkFP 200 0 Nothing) Nothing `shouldBe` 170

  it "applies both discounts together" $ do
    -- 300 - 30 - 20 = 250
    let fp = withBusinessDiscount 30 . withPersonalDiscount 20 $ mkFP 300 0 Nothing
    fareSum fp Nothing `shouldBe` 250

  it "combines driver fare, customer fee, and discounts" $ do
    -- pureFareSum(200) + 25 + 10 - 15 = 220
    let fp = withDriverSelectedFare 25 . withCustomerExtraFee 10 . withBusinessDiscount 15 $ mkFP 200 0 Nothing
    fareSum fp Nothing `shouldBe` 220

-- ---------------------------------------------------------------------------
-- isNightShift
-- ---------------------------------------------------------------------------

nightShiftSpecs :: Spec
nightShiftSpecs = do
  it "returns True during night hours (11 PM IST)" $ do
    -- 11 PM IST = 5:30 PM UTC = 17:30 UTC
    let time = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 17 30 0)
    isNightShift standardNightShiftBounds time `shouldBe` True

  it "returns True during early morning (2 AM IST)" $ do
    -- 2 AM IST = 8:30 PM UTC prev day = 20:30 UTC
    let time = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 20 30 0)
    isNightShift standardNightShiftBounds time `shouldBe` True

  it "returns False during daytime (2 PM IST)" $ do
    -- 2 PM IST = 8:30 AM UTC
    let time = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 8 30 0)
    isNightShift standardNightShiftBounds time `shouldBe` False

  it "returns False during daytime (10 AM IST)" $ do
    -- 10 AM IST = 4:30 AM UTC
    let time = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 4 30 0)
    isNightShift standardNightShiftBounds time `shouldBe` False

  it "returns False at exactly night shift end (5 AM IST)" $ do
    -- 5 AM IST = 11:30 PM UTC = 23:30 UTC
    let time = UTCTime (fromGregorian 2024 1 14) (timeOfDayToTime $ TimeOfDay 23 30 0)
    isNightShift standardNightShiftBounds time `shouldBe` False

  it "returns False at exactly night shift start (10 PM IST)" $ do
    -- 10 PM IST = 4:30 PM UTC = 16:30 UTC
    let time = UTCTime (fromGregorian 2024 1 15) (timeOfDayToTime $ TimeOfDay 16 30 0)
    -- isNightShift uses strict comparison (startTime < time), so exactly at start is False
    isNightShift standardNightShiftBounds time `shouldBe` False

-- ---------------------------------------------------------------------------
-- calculateCancellationCharges
-- ---------------------------------------------------------------------------

cancellationChargeSpecs :: Spec
cancellationChargeSpecs = do
  let policy = mkCancellationFarePolicy

  it "returns min charge when no distance traveled" $ do
    calculateCancellationCharges policy (Just $ Meters 5000) (Just $ Meters 5000) 300 200
      `shouldBe` 25

  it "returns min charge when distances are Nothing" $ do
    calculateCancellationCharges policy Nothing Nothing 300 200
      `shouldBe` 25

  it "calculates distance + time based charges" $ do
    -- Initial dist: 5000m, current dist: 2000m => traveled: 3000m
    -- Distance charges: 3000 * 0.005 = 15
    -- Time: 600s => 10 mins * 2 = 20
    -- Total: 35, percentageOfFare: 0.5 * 200 = 100
    -- max(25, min(35, min(100, 100))) = 35
    calculateCancellationCharges policy (Just $ Meters 5000) (Just $ Meters 2000) 600 200
      `shouldBe` 35

  it "caps at max cancellation charge" $ do
    -- traveled: 10000m => 50, time: 3600s => 120, total: 170, capped at 100
    calculateCancellationCharges policy (Just $ Meters 15000) (Just $ Meters 5000) 3600 500
      `shouldBe` 100

  it "caps at percentage of ride fare when that is lower" $ do
    -- traveled: 5000m => 25, time: 600s => 20, total: 45
    -- percentageOfFare: 0.5 * 50 = 25
    -- max(25, min(45, min(25, 100))) = 25
    calculateCancellationCharges policy (Just $ Meters 10000) (Just $ Meters 5000) 600 50
      `shouldBe` 25

-- ---------------------------------------------------------------------------
-- calculateNoShowCharges
-- ---------------------------------------------------------------------------

noShowChargeSpecs :: Spec
noShowChargeSpecs = do
  let policy = mkCancellationFarePolicy

  it "returns Nothing when driver arrival time is Nothing" $ do
    calculateNoShowCharges Nothing (Just policy) dayTime `shouldBe` Nothing

  it "returns Nothing when cancellation policy is Nothing" $ do
    calculateNoShowCharges (Just dayTime) Nothing dayTime `shouldBe` Nothing

  it "returns Nothing when waiting time is within limit" $ do
    -- Driver arrived 2 minutes ago (120s), maxWaiting = 300s
    let arrivalTime = addUTCTime (-120) dayTime
    calculateNoShowCharges (Just arrivalTime) (Just policy) dayTime `shouldBe` Nothing

  it "charges when waiting time exceeds limit" $ do
    -- Driver arrived 10 minutes ago (600s), maxWaiting = 300s
    -- maxCancellationCharge(100) + (300/60=5 mins * 2) = 110
    let arrivalTime = addUTCTime (-600) dayTime
    calculateNoShowCharges (Just arrivalTime) (Just policy) dayTime `shouldBe` Just 110

-- ---------------------------------------------------------------------------
-- calculateBusinessDiscount
-- ---------------------------------------------------------------------------

businessDiscountSpecs :: Spec
businessDiscountSpecs = do
  it "calculates percentage discount on ride cost" $ do
    -- fullRideCost = 200, discount = 200 * 10 / 100 = 20
    calculateBusinessDiscount (mkFP 200 0 Nothing) 10 `shouldBe` Just 20

  it "includes extraKmFare and deadKmFare in discount base" $ do
    -- fullRideCost = 100 + 70 + 30 = 200, discount = 200 * 5 / 100 = 10
    calculateBusinessDiscount (mkFP 100 30 (Just 70)) 5 `shouldBe` Just 10

  it "includes night shift and congestion in discount base" $ do
    -- fullRideCost = 200 + 30 + 50 = 280, discount = 280 * 10 / 100 = 28
    let fp = withNightShiftCharge 50 . withCongestionCharge 30 $ mkFP 200 0 Nothing
    calculateBusinessDiscount fp 10 `shouldBe` Just 28

  it "returns zero discount for 0 percentage" $ do
    calculateBusinessDiscount (mkFP 200 0 Nothing) 0 `shouldBe` Just 0

-- ---------------------------------------------------------------------------
-- Integration: full fare scenarios
-- ---------------------------------------------------------------------------

integrationSpecs :: Spec
integrationSpecs = do
  it "short ride (2km): base fare + dead km fare" $ do
    let fp = mkFP 30 20 Nothing
    pureFareSum fp Nothing `shouldBe` 50
    fareSum fp Nothing `shouldBe` 50

  it "long ride (50km): base fare + significant extra km fare" $ do
    -- 50km ride: baseFare 30, deadKm 20, extraKm for 48km at 12/km = 576
    let fp = mkFP 30 20 (Just 576)
    pureFareSum fp Nothing `shouldBe` 626
    fareSum fp Nothing `shouldBe` 626

  it "surge pricing (2x via congestion multiplier)" $ do
    -- Base ride cost: 100 + 50 (extraKm) = 150
    -- 2x surge: congestion = 150
    let fp = withCongestionCharge 150 $ mkFP 100 0 (Just 50)
    pureFareSum fp Nothing `shouldBe` 300

  it "night charges: progressive night shift" $ do
    -- Base: 100, extraKm: 80, nightShift: 1.5x on (100+80=180) => 90
    let fp = withNightShiftCharge 90 $ mkFP 100 0 (Just 80)
    pureFareSum fp Nothing `shouldBe` 270

  it "night charges: constant night shift" $ do
    let fp = withNightShiftCharge 50 $ mkFP 100 20 Nothing
    pureFareSum fp Nothing `shouldBe` 170

  it "toll route" $ do
    let fp = withTollCharges 45 $ mkFP 80 15 (Just 60)
    pureFareSum fp Nothing `shouldBe` 200

  it "toll route with VAT" $ do
    let fp = withTollVat 11.25 . withTollCharges 45 $ mkFP 80 15 (Just 60)
    -- 80 + 15 + 60 + 45 + 11.25 = 211.25
    pureFareSum fp Nothing `shouldBe` 211.25

  it "waiting time charge" $ do
    let fp = withWaitingCharge 30 $ mkFP 100 20 (Just 40)
    pureFareSum fp Nothing `shouldBe` 190

  it "complex ride: night + surge + tolls + waiting" $ do
    let fp =
          withNightShiftCharge 60
            . withCongestionCharge 40
            . withTollCharges 55
            . withWaitingCharge 15
            . withServiceCharge 10
            $ mkFP 120 25 (Just 85)
    -- 120 + 10 + 15 + 60 + 40 + 85 + 25 + 55 = 410
    pureFareSum fp Nothing `shouldBe` 410

  it "ride with business discount" $ do
    let fp = withBusinessDiscount 30 $ mkFP 200 0 (Just 100)
    pureFareSum fp Nothing `shouldBe` 300
    fareSum fp Nothing `shouldBe` 270

  it "ride with both discounts and driver fare" $ do
    let fp =
          withBusinessDiscount 20
            . withPersonalDiscount 15
            . withDriverSelectedFare 10
            $ mkFP 200 30 (Just 70)
    pureFareSum fp Nothing `shouldBe` 300
    fareSum fp Nothing `shouldBe` 275

  it "ride with all V2 charges (VAT, toll VAT, processing fee)" $ do
    let fp =
          withTollCharges 40
            . withRideVat 35
            . withTollVat 10
            . withPaymentProcessingFee 5
            $ mkFP 150 20 (Just 80)
    -- 150 + 20 + 80 + 40 + 35 + 10 + 5 = 340
    pureFareSum fp Nothing `shouldBe` 340

  it "ride with luggage and pet charges" $ do
    let fp = withLuggageCharge 20 . withPetCharges 30 $ mkFP 100 15 Nothing
    pureFareSum fp Nothing `shouldBe` 165

  it "ride with insurance (per-distance)" $ do
    let fp = withInsuranceCharge 8 $ mkFP 100 20 (Just 60)
    pureFareSum fp Nothing `shouldBe` 188

  it "ride with stop charges and driver allowance" $ do
    let fp = withStopCharges 16 . withDriverAllowance 25 $ mkFP 100 15 (Just 40)
    pureFareSum fp Nothing `shouldBe` 196

  it "ride with extra time fare" $ do
    let fp = withRideExtraTimeFare 18 $ mkFP 100 20 (Just 50)
    pureFareSum fp Nothing `shouldBe` 188
