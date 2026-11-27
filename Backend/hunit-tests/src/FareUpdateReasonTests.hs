{-# LANGUAGE OverloadedStrings #-}

-- | Golden cases for the end-ride fare-change sentence
-- (rider-app SharedLogic.FareUpdateReason), mirroring the consumer app's
-- fareUpdatedText / computeFareBreakdown behaviour it replaces.
module FareUpdateReasonTests (tests) where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.Settlement as Settlement
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Domain.Types.FareBreakup (FareBreakupAPIEntity (..))
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney (..), Money (..), PriceAPIEntity (..), Seconds (..))
import SharedLogic.FareUpdateReason
import Test.Tasty
import Test.Tasty.HUnit

price :: Currency -> Rational -> PriceAPIEntity
price currency amount = PriceAPIEntity {amount = HighPrecMoney amount, currency = currency}

line :: Text -> Rational -> FareBreakupAPIEntity
line description amount =
  FareBreakupAPIEntity
    { amount = Money (round amount),
      amountWithCurrency = price INR amount,
      description = description
    }

rideStart :: UTCTime
rideStart = UTCTime (fromGregorian 2026 11 1) 0

-- | ₹100 estimate, 5 km estimated, 7.3 km charged, ₹140 final.
base :: FareUpdateInput
base =
  FareUpdateInput
    { finalFareBreakup = [],
      estimatedFareBreakup = [],
      grossMultipliers = Nothing,
      computedPrice = Just (price INR 140),
      estimatedTotalFare = price INR 100,
      rideStartTime = Nothing,
      rideEndTime = Nothing,
      estimatedDuration = Nothing,
      estimatedDistance = Just 5000,
      chargeableDistance = Just 7300,
      isRental = False
    }

sentence :: FareUpdateInput -> Maybe Text
sentence input = runIdentity . renderFareUpdateText Lang.ENGLISH (pure . englishText) <$> computeFareUpdate input

(@?=.) :: FareUpdateInput -> Maybe Text -> Assertion
input @?=. expected = sentence input @?= expected

tests :: TestTree
tests =
  testGroup
    "FareUpdateReason"
    [ testGroup
        "no change shown"
        [ testCase "final fare equals estimate" $
            base {computedPrice = Just (price INR 100)} @?=. Nothing,
          testCase "sub-rupee difference is not visible" $
            base {computedPrice = Just (price INR 104.4), estimatedTotalFare = price INR 104} @?=. Nothing,
          testCase "no final price yet" $
            base {computedPrice = Nothing} @?=. Nothing,
          testCase "toll on the final fare only is excluded" $
            base {finalFareBreakup = [line "TOLL_CHARGES" 40]} @?=. Nothing,
          testCase "toll is grossed up by the toll multiplier" $
            let multipliers = (pure (Settlement.GrossMultiplier 1 1)) {FB.toll = Settlement.GrossMultiplier 1.05 1}
             in base {finalFareBreakup = [line "TOLL_CHARGES" 40], computedPrice = Just (price INR 142), grossMultipliers = Just multipliers} @?=. Nothing
        ],
      testGroup
        "non-rental"
        [ testCase "increase, ride longer" $
            base @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km longer",
          testCase "increase but ride shorter: no consistent reason" $
            base {chargeableDistance = Just 4200} @?=. Just "Fare increased by \8377\&40",
          testCase "decrease, ride shorter (under 1 km)" $
            base {computedPrice = Just (price INR 90), chargeableDistance = Just 4200}
              @?=. Just "Fare reduced by \8377\&10 as the ride was 800 m shorter",
          testCase "decrease never blamed on waiting" $
            base {computedPrice = Just (price INR 90), chargeableDistance = Just 5000, finalFareBreakup = [line "WAITING_OR_PICKUP_CHARGES" 20]}
              @?=. Just "Fare reduced by \8377\&10",
          testCase "increase with waiting charge" $
            base {finalFareBreakup = [line "WAITING_OR_PICKUP_CHARGES" 20]}
              @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km longer and wait time was longer",
          testCase "missing chargeable distance reads as shorter (app behaviour)" $
            base {computedPrice = Just (price INR 90), chargeableDistance = Nothing}
              @?=. Just "Fare reduced by \8377\&10 as the ride was 5.0 km shorter"
        ],
      testGroup
        "non-rental ride time"
        [ testCase "extra-time charge and longer distance" $
            timed {finalFareBreakup = [line "EXTRA_TIME_FARE" 24]}
              @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km and 12 min longer",
          testCase "extra-time charge only" $
            timed {finalFareBreakup = [line "EXTRA_TIME_FARE" 24], chargeableDistance = Just 5000}
              @?=. Just "Fare increased by \8377\&40 as the ride was 12 min longer",
          testCase "ride duration fare re-priced upward" $
            timed
              { estimatedFareBreakup = [line "RIDE_DURATION_FARE" 20],
                finalFareBreakup = [line "RIDE_DURATION_FARE" 40],
                chargeableDistance = Just 5000
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 12 min longer",
          testCase "ride duration fare unchanged: time not named" $
            timed
              { estimatedFareBreakup = [line "RIDE_DURATION_FARE" 20],
                finalFareBreakup = [line "RIDE_DURATION_FARE" 20],
                chargeableDistance = Just 5000
              }
              @?=. Just "Fare increased by \8377\&40",
          testCase "a longer ride with no time charge names distance only" $
            timed @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km longer",
          testCase "under a minute over is not extra time" $
            timed
              { finalFareBreakup = [line "EXTRA_TIME_FARE" 24],
                chargeableDistance = Just 5000,
                rideEndTime = Just (addUTCTime (1200 + 30) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40",
          testCase "no estimated duration: time not named" $
            timed {finalFareBreakup = [line "EXTRA_TIME_FARE" 24], chargeableDistance = Just 5000, estimatedDuration = Nothing}
              @?=. Just "Fare increased by \8377\&40",
          testCase "extra time with waiting" $
            timed {finalFareBreakup = [line "EXTRA_TIME_FARE" 24, line "WAITING_OR_PICKUP_CHARGES" 5], chargeableDistance = Just 5000}
              @?=. Just "Fare increased by \8377\&40 as the ride was 12 min longer and wait time was longer",
          testCase "distance, time and waiting all named" $
            timed {finalFareBreakup = [line "EXTRA_TIME_FARE" 24, line "WAITING_OR_PICKUP_CHARGES" 5]}
              @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km and 12 min longer and wait time was longer",
          testCase "a decrease is never blamed on time" $
            timed {computedPrice = Just (price INR 90), chargeableDistance = Just 4200, finalFareBreakup = [line "EXTRA_TIME_FARE" 24]}
              @?=. Just "Fare reduced by \8377\&10 as the ride was 800 m shorter"
        ],
      testGroup
        "tip"
        [ testCase "a tip alone is not a fare increase" $
            base {chargeableDistance = Just 5000, finalFareBreakup = [line "BUYER_ADDITIONAL_AMOUNT" 40]}
              @?=. Nothing,
          testCase "a tip is left out of a real increase" $
            base {computedPrice = Just (price INR 160), finalFareBreakup = [line "BUYER_ADDITIONAL_AMOUNT" 20]}
              @?=. Just "Fare increased by \8377\&40 as the ride was 2.3 km longer"
        ],
      testGroup
        "rental"
        [ testCase "decrease" $
            rental {computedPrice = Just (price INR 90)} @?=. Just "Fare reduced by \8377\&10 as the ride was shorter",
          testCase "extra distance and time" $
            rental
              { finalFareBreakup = [line "DIST_BASED_FARE" 30, line "TIME_BASED_FARE" 10],
                estimatedDistance = Just 10000,
                chargeableDistance = Just 13000,
                rideEndTime = Just (addUTCTime (3600 + 4500) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 3.0 km and 1 hr 15 min longer",
          testCase "extra time only, with waiting" $
            rental
              { finalFareBreakup = [line "TIME_BASED_FARE" 10, line "WAITING_OR_PICKUP_CHARGES" 5],
                rideEndTime = Just (addUTCTime (3600 + 1200) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 20 min longer and wait time was longer",
          testCase "distance and time suppress the waiting reason" $
            rental
              { finalFareBreakup = [line "DIST_BASED_FARE" 30, line "TIME_BASED_FARE" 10, line "WAITING_OR_PICKUP_CHARGES" 5],
                estimatedDistance = Just 10000,
                chargeableDistance = Just 13000,
                rideEndTime = Just (addUTCTime (3600 + 7200) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 3.0 km and 2 hr longer",
          testCase "under a minute over is not extra time (no \"0 min longer\")" $
            rental
              { finalFareBreakup = [line "TIME_BASED_FARE" 10],
                rideEndTime = Just (addUTCTime (3600 + 30) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40",
          testCase "exactly one minute over is extra time" $
            rental
              { finalFareBreakup = [line "TIME_BASED_FARE" 10],
                rideEndTime = Just (addUTCTime (3600 + 60) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 1 min longer",
          testCase "a shorter ride is never called longer" $
            rental
              { finalFareBreakup = [line "DIST_BASED_FARE" 30],
                estimatedDistance = Just 10000,
                chargeableDistance = Just 8000
              }
              @?=. Just "Fare increased by \8377\&40",
          testCase "a shorter ride still gets its time reason" $
            rental
              { finalFareBreakup = [line "DIST_BASED_FARE" 30, line "TIME_BASED_FARE" 10],
                estimatedDistance = Just 10000,
                chargeableDistance = Just 8000,
                rideEndTime = Just (addUTCTime (3600 + 1200) rideStart)
              }
              @?=. Just "Fare increased by \8377\&40 as the ride was 20 min longer"
        ],
      testGroup
        "currency and formatting"
        [ testCase "EUR keeps 2 decimals" $
            base {computedPrice = Just (price EUR 12.5), estimatedTotalFare = price EUR 10.25}
              @?=. Just "Fare increased by \8364\&2.25 as the ride was 2.3 km longer",
          testCase "Dutch rider: comma decimals" $ do
            formatDistanceText Lang.DUTCH EUR 1500 @?= "1,5 km"
            formatAmountText Lang.DUTCH EUR 2.25 @?= "\8364\&2,25",
          testCase "JOD keeps 3 decimals (the app would have shown rupees)" $
            base {computedPrice = Just (price JOD 12.3456), estimatedTotalFare = price JOD 10}
              @?=. Just "Fare increased by JD 2.346 as the ride was 2.3 km longer",
          testCase "INR uses Indian grouping" $
            formatAmountText Lang.ENGLISH INR 123456 @?= "\8377\&1,23,456",
          testCase "EUR in English uses Western grouping" $
            formatAmountText Lang.ENGLISH EUR 1234.5 @?= "\8364\&1,234.50",
          testCase "extra time formatting" $ do
            formatExtraTimeText 59 @?= "0 min"
            formatExtraTimeText 1500 @?= "25 min"
            formatExtraTimeText 3600 @?= "1 hr"
            formatExtraTimeText 3900 @?= "1 hr 5 min"
        ]
    ]
  where
    -- The non-rental base with a 20 min estimate and a 32 min ride (12 min over).
    timed =
      base
        { estimatedDuration = Just (Seconds 1200),
          rideStartTime = Just rideStart,
          rideEndTime = Just (addUTCTime (1200 + 720) rideStart)
        }
    -- ₹100 estimate, 1 h estimated, 10 km estimated and charged, ₹140 final.
    rental =
      base
        { isRental = True,
          estimatedDistance = Just 10000,
          chargeableDistance = Just 10000,
          estimatedDuration = Just (Seconds 3600),
          rideStartTime = Just rideStart,
          rideEndTime = Just (addUTCTime 3600 rideStart)
        }
