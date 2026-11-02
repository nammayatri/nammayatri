{-# LANGUAGE OverloadedStrings #-}

-- | Driver earnings fare breakup (SharedLogic.FareBreakup): coverage of
-- fareSum, config validation, largest-remainder rounding and line hiding.
module FareBreakupTests (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.Extra.ConditionalCharges as DACE
import Domain.Types.Extra.TransporterConfig (FareBreakupNode (..))
import Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy as FP
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), DistanceUnit (..), HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Lib.Types.SpecialLocation (FareSettlementType (..))
import SharedLogic.FareBreakup
import SharedLogic.FareCalculator (fareSum)
import Test.Tasty
import Test.Tasty.HUnit

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

progressive :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> FareParametersDetails
progressive deadKm extraKm duration = ProgressiveDetails (FParamsProgressiveDetails deadKm (Just extraKm) (Just duration) INR)

-- | Every field is spelled out on purpose: a new FareParameters field fails to
-- compile here (-Wmissing-fields under -Werror) until it is set, and the
-- coverage test then fails until 'buildComponentMap' has a component for it.
zeroParams :: FareParameters
zeroParams =
  FareParameters
    { id = Id "fp-1",
      driverSelectedFare = Nothing,
      customerExtraFee = Nothing,
      negativeFareAdjustment = Nothing,
      serviceCharge = Nothing,
      parkingCharge = Nothing,
      stopCharges = Nothing,
      govtCharges = Nothing,
      baseFare = 0,
      waitingCharge = Nothing,
      rideExtraTimeFare = Nothing,
      nightShiftCharge = Nothing,
      nightShiftRateIfApplies = Nothing,
      fareParametersDetails = progressive 0 0 0,
      customerCancellationDues = Nothing,
      tollCharges = Nothing,
      congestionCharge = Nothing,
      petCharges = Nothing,
      driverAllowance = Nothing,
      airportConvenienceFee = Nothing,
      businessDiscount = Nothing,
      personalDiscount = Nothing,
      priorityCharges = Nothing,
      congestionChargeViaDp = Nothing,
      insuranceCharge = Nothing,
      cardCharge = Nothing,
      luggageCharge = Nothing,
      returnFeeCharge = Nothing,
      boothCharge = Nothing,
      schedulingCharge = Nothing,
      platformFee = Nothing,
      sgst = Nothing,
      cgst = Nothing,
      tdsAmount = Nothing,
      tdsRate = Nothing,
      tdsProcessedAt = Nothing,
      platformFeeChargesBy = FP.None,
      currency = INR,
      updatedAt = epoch,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing,
      conditionalCharges = [],
      customerGateFeeItems = Nothing,
      shouldApplyBusinessDiscount = False,
      shouldApplyPersonalDiscount = False,
      driverCancellationNotAllowed = Nothing,
      paymentProcessingFee = Nothing,
      paymentProcessingFeeVat = Nothing,
      isVatTaxType = Nothing,
      discountApplicableRideFareTaxExclusive = Nothing,
      discountApplicableRideFareTax = Nothing,
      nonDiscountApplicableRideFareTaxExclusive = Nothing,
      nonDiscountApplicableRideFareTax = Nothing,
      tollFareTaxExclusive = Nothing,
      tollFareTax = Nothing,
      cancellationFeeTaxExclusive = Nothing,
      cancellationTax = Nothing,
      parkingChargeTaxExclusive = Nothing,
      parkingChargeTax = Nothing,
      fareSettlementType = Nothing,
      negotiatedFareDelta = Nothing,
      bufferedFare = Nothing,
      addOnCharges = Nothing
    }

-- | A distinct non-zero amount in every charge 'fareSum' counts.
allChargesParams :: FareParametersDetails -> FareParameters
allChargesParams details =
  zeroParams
    { driverSelectedFare = Just 1,
      customerExtraFee = Just 2,
      negativeFareAdjustment = Just (-3),
      serviceCharge = Just 4,
      parkingCharge = Just 5,
      stopCharges = Just 6,
      govtCharges = Just 7,
      baseFare = 8,
      waitingCharge = Just 9,
      rideExtraTimeFare = Just 10,
      nightShiftCharge = Just 11,
      fareParametersDetails = details,
      customerCancellationDues = Just 12,
      tollCharges = Just 13,
      congestionCharge = Just 14,
      petCharges = Just 15,
      driverAllowance = Just 16,
      airportConvenienceFee = Just 17,
      priorityCharges = Just 18,
      insuranceCharge = Just 19,
      cardCharge = Just (CardCharge (Just 20) (Just 21)),
      luggageCharge = Just 22,
      returnFeeCharge = Just 23,
      boothCharge = Just 24,
      schedulingCharge = Just 25,
      conditionalCharges = [DAC.ConditionalCharges 0 26 DACE.SAFETY_PLUS_CHARGES "fp" 0 epoch epoch],
      customerGateFeeItems = Just [CustomerGateFeeItem "gate" 27],
      paymentProcessingFee = Just 28,
      paymentProcessingFeeVat = Just 29,
      tollFareTax = Just 30,
      parkingChargeTax = Just 31,
      negotiatedFareDelta = Just 32,
      addOnCharges = Just 33
    }

detailVariants :: [(String, FareParametersDetails)]
detailVariants =
  [ ("progressive", progressive 40 41 42),
    ("slab", SlabDetails (FParamsSlabDetails (Just 43) (Just 44) (Just 45) INR)),
    ("rental", RentalDetails (FParamsRentalDetails 46 47 INR 0 Meter 0 48)),
    ("intercity", InterCityDetails (FParamsInterCityDetails 49 50 51 INR 52 (Just 53) 54)),
    ("ambulance", AmbulanceDetails (FParamsAmbulanceDetails (Just 55) (Just 56) (Just 57) 58 INR))
  ]

leaf :: Text -> [FP.FareChargeComponent] -> FareBreakupNode
leaf key tags = FareBreakupNode {key, fareTags = Just tags, subItems = Nothing}

branch :: Text -> [FareBreakupNode] -> FareBreakupNode
branch key kids = FareBreakupNode {key, fareTags = Nothing, subItems = Just kids}

-- | (key, amount) of a computed line and its shown children.
shape :: FareBreakupLine -> (Text, HighPrecMoney, [(Text, HighPrecMoney)])
shape line = (line.key, line.amount, [(kid.key, kid.amount) | kid <- line.subItems])

tests :: TestTree
tests =
  testGroup
    "FareBreakup"
    [ testGroup "fare tags cover fareSum" (map coversFareSum detailVariants),
      testCase "parking is not a fare amount when the booth EDC collects it" $
        let fp = (allChargesParams (progressive 40 41 42)) {fareSettlementType = Just ParkingOnly}
         in sum (Map.elems (breakupComponentAmounts fp)) @?= fareSum fp Nothing - 28 - 29,
      testGroup
        "config validation"
        [ testCase "valid config" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "TOLL" [FP.TollChargesComponent]]) @?= [],
          testCase "RideVatComponent is rejected: it is inside GovtChargesComponent" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "TAX" [FP.RideVatComponent]])
              @?= ["TOTAL_FARE > TAX: RideVatComponent is already included in GovtChargesComponent"],
          testCase "tag in two leaves" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [leaf "A" [FP.RideFare], leaf "B" [FP.RideFare]])
              @?= ["fare tag RideFare is listed in more than one leaf"],
          testCase "both fareTags and subItems" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [FareBreakupNode {key = "A", fareTags = Just [FP.RideFare], subItems = Just [leaf "B" [FP.TollChargesComponent]]}])
              @?= ["TOTAL_FARE > A: exactly one of fareTags or subItems must be non-empty"],
          testCase "empty group" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [branch "A" []])
              @?= ["TOTAL_FARE > A: exactly one of fareTags or subItems must be non-empty"],
          testCase "empty key" $
            validateFareBreakupConfig (branch "TOTAL_FARE" [leaf " " [FP.RideFare]])
              @?= ["TOTAL_FARE >  : key must not be empty"],
          testCase "nested too deep" $
            validateFareBreakupConfig (branch "L1" [branch "L2" [branch "L3" [branch "L4" [leaf "L5" [FP.RideFare]]]]])
              @?= ["L1 > L2 > L3 > L4 > L5: nested deeper than 4 levels"]
        ],
      testGroup
        "computed breakup"
        [ testCase "largest remainder: 2.5 + 2.3 + (2.1 + 4.8) shows as 3 + 2 + (2 + 5) = 12" $
            let fp =
                  zeroParams
                    { baseFare = 2.5,
                      waitingCharge = Just 2.3,
                      tollCharges = Just 2.1,
                      fareParametersDetails = progressive 0 4.8 0
                    }
                cfg = branch "TOTAL_FARE" [leaf "A" [FP.RideFare], leaf "B" [FP.WaitingCharge], branch "REST" [leaf "C" [FP.TollChargesComponent], leaf "D" [FP.ExtraKmFareComponent]]]
             in case computeFareBreakup INR cfg fp of
                  Left err -> assertFailure (T.unpack err)
                  Right line -> do
                    shape line @?= ("TOTAL_FARE", 12, [("A", 3), ("B", 2), ("REST", 7)])
                    map shape (filter (\kid -> kid.key == "REST") line.subItems) @?= [("REST", 7, [("C", 2), ("D", 5)])],
          testCase "image example: negotiation is a negative leaf" $
            let fp = zeroParams {baseFare = 160, negotiatedFareDelta = Just (-10), waitingCharge = Just 20, tollCharges = Just 40, fareParametersDetails = SlabDetails (FParamsSlabDetails Nothing (Just 11) (Just 11) INR)}
                cfg =
                  branch
                    "FARE_PAID_BY_CUSTOMER"
                    [ branch "RIDE_FARE" [leaf "DISTANCE_FARE" [FP.RideFare, FP.DistBasedFareComponent, FP.ExtraKmFareComponent], leaf "NEGOTIATION" [FP.NegotiatedFareDeltaComponent]],
                      leaf "WAITING_CHARGES" [FP.WaitingCharge],
                      leaf "TOLL_CHARGES" [FP.TollChargesComponent],
                      leaf "TAX" [FP.CgstComponent, FP.SgstComponent]
                    ]
             in fmap shape (computeFareBreakup INR cfg fp)
                  @?= Right ("FARE_PAID_BY_CUSTOMER", 232, [("RIDE_FARE", 150), ("WAITING_CHARGES", 20), ("TOLL_CHARGES", 40), ("TAX", 22)]),
          testCase "zero lines are hidden" $
            let fp = zeroParams {baseFare = 100, waitingCharge = Just 0.3, tollCharges = Just 40}
                cfg = branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "WAITING" [FP.WaitingCharge], leaf "TOLL" [FP.TollChargesComponent]]
             in fmap shape (computeFareBreakup INR cfg fp) @?= Right ("TOTAL_FARE", 140, [("RIDE", 100), ("TOLL", 40)]),
          testCase "a lone group keeps its subtree (real local ride: 36 base + 67.554 extra km + 10 dead km + 15.5331 congestion)" $
            let fp = zeroParams {baseFare = 36, congestionCharge = Just 15.5331, fareParametersDetails = progressive 10 67.554 0}
                cfg =
                  branch
                    "FARE_PAID_BY_CUSTOMER"
                    [ branch "RIDE_FARE" [leaf "DISTANCE_FARE" [FP.RideFare, FP.ExtraKmFareComponent, FP.DeadKmFareComponent], leaf "CONGESTION_CHARGE" [FP.CongestionChargeComponent], leaf "NEGOTIATION" [FP.NegotiatedFareDeltaComponent]],
                      leaf "WAITING_CHARGES" [FP.WaitingCharge],
                      leaf "TOLL_CHARGES" [FP.TollChargesComponent],
                      leaf "TAX" [FP.CgstComponent, FP.SgstComponent, FP.GovtChargesComponent, FP.TollVatComponent]
                    ]
             in case computeFareBreakup INR cfg fp of
                  Left err -> assertFailure (T.unpack err)
                  Right line -> do
                    shape line @?= ("FARE_PAID_BY_CUSTOMER", 129, [("RIDE_FARE", 129)])
                    map shape line.subItems @?= [("RIDE_FARE", 129, [("DISTANCE_FARE", 114), ("CONGESTION_CHARGE", 15)])],
          testCase "a level whose only line is a leaf is not expanded" $
            let fp = zeroParams {baseFare = 100}
                cfg = branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "TOLL" [FP.TollChargesComponent]]
             in fmap shape (computeFareBreakup INR cfg fp) @?= Right ("TOTAL_FARE", 100, []),
          testCase "a charge no leaf lists skips the breakup" $
            let fp = zeroParams {baseFare = 100, petCharges = Just 30}
                cfg = branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "TOLL" [FP.TollChargesComponent]]
             in either (const True) (const False) (computeFareBreakup INR cfg fp) @?= True,
          testCase "a VAT ride counts tax once (ride VAT sits inside govtCharges)" $
            let fp = zeroParams {baseFare = 100, govtCharges = Just 10, discountApplicableRideFareTax = Just 8}
                cfg = branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare], leaf "TAX" [FP.GovtChargesComponent]]
             in fmap shape (computeFareBreakup INR cfg fp) @?= Right ("TOTAL_FARE", 110, [("RIDE", 100), ("TAX", 10)]),
          testCase "a listed tag with no amount on this ride is fine" $
            let fp = zeroParams {baseFare = 100, tollCharges = Just 20}
                cfg = branch "TOTAL_FARE" [leaf "RIDE" [FP.RideFare, FP.TimeFareComponent], leaf "TOLL" [FP.TollChargesComponent]]
             in fmap shape (computeFareBreakup INR cfg fp) @?= Right ("TOTAL_FARE", 120, [("RIDE", 100), ("TOLL", 20)])
        ]
    ]

-- | The fare tags add up to 'fareSum', less the payment processing fee and its
-- VAT, which have their own earnings rows.
coversFareSum :: (String, FareParametersDetails) -> TestTree
coversFareSum (name, details) =
  testCase name $
    let fp = allChargesParams details
     in sum (Map.elems (breakupComponentAmounts fp)) @?= fareSum fp Nothing - 28 - 29
