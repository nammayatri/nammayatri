{-
  Lib.Finance.Invoice.LineItems

  Shared itemized-receipt line-item assembly: classifies a ride's
  (fareBreakup tag, amount) pairs into displayable InvoiceLineItems with the
  consumer app's exact labels, order and sign (a faithful port of
  getInvoiceFare in ny-react-native fareEntityHelper.ts).

  Moved from the per-app SharedLogic.Finance.RideReceipt twins (rider-app +
  dynamic-offer-driver-app) so the classification lives in one place. Each app
  feeds its own fare source reduced to [(Text, HighPrecMoney)] — BAP: ride
  fareBreakup rows; BPP: FareParameters via FareCalculator.mkFareParamsBreakups.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.LineItems
  ( mkLineItemsFromTaggedAmounts,
  )
where

import qualified Data.Char as Char
import Data.List (nubBy, sortOn)
import qualified Data.Text as T
import Domain.SharedLogic.RideDiscount (isProjectedFareParamTag)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), ItemType (..), LineItemDescription (..))

data RowKind = Charge | Discount | AnyCharge

-- | (isSpecialZone, fare (tag, amount) pairs) -> receipt line items. Known charge
-- tags render with the app's exact label when amount > 0; discount tags render as
-- negative adjustments; rate/config/projected tags are dropped; unknown tags are
-- title-cased and kept when non-zero. Sorted by the app's invoiceOrder. Every item
-- carries itemType (parseLineItems errors on untyped items).
mkLineItemsFromTaggedAmounts :: Bool -> [(Text, HighPrecMoney)] -> [InvoiceLineItem]
mkLineItemsFromTaggedAmounts isSpecialZone taggedAmounts =
  dedupe . map snd . sortOn fst $ mapMaybe toRow taggedAmounts
  where
    -- FareCalculator emits ONDC v2.1.0 duplicate tags from the same source field
    -- (PARKING_CHARGE+PARKING_CHARGES, NIGHT_SHIFT_CHARGE+NIGHT_CHARGES,
    -- WAITING_OR_PICKUP_CHARGES+WAITING_CHARGES); without this collapse the pair
    -- renders twice and inflates finalAmount past what the customer paid.
    dedupe = nubBy (\a b -> a.description == b.description && a.lineTotal == b.lineTotal)
    toRow :: (Text, HighPrecMoney) -> Maybe (Int, InvoiceLineItem)
    toRow (tag, amt) =
      classifyTag isSpecialZone tag >>= \(label, descType, order, kind) ->
        case kind of
          Charge | amt > 0 -> Just (order, mkRow label descType amt Fare)
          Discount | amt > 0 -> Just (order, mkRow label descType (negate amt) Adjustment)
          AnyCharge | amt /= 0 -> Just (order, mkRow label descType amt Fare)
          _ -> Nothing
    mkRow label descType amt itemTy =
      InvoiceLineItem
        { description = label,
          descriptionType = descType,
          quantity = 1,
          unitPrice = amt,
          lineTotal = amt,
          isExternalCharge = False,
          groupId = Nothing,
          itemType = Just itemTy
        }

classifyTag :: Bool -> Text -> Maybe (Text, Maybe LineItemDescription, Int, RowKind)
classifyTag isSpecialZone tag
  | isProjectedFareParamTag tag = Nothing
  | "EXTRA_PER_KM_STEP_FARE_" `T.isPrefixOf` tag = Nothing
  | tag `elem` droppedTags = Nothing
  | otherwise = case tag of
    "BASE_FARE" -> charge "Base Fare" BaseFare 0
    "EXTRA_DISTANCE_FARE" -> charge "Optional Driver Request" OptionalDriverRequest 1
    "DISTANCE_FARE" -> charge "Distance Fare" DistanceFare 2
    "DIST_BASED_FARE" -> charge "Distance Based Charges" DistanceBasedCharges 3
    "TIME_BASED_FARE" -> charge "Time Based Charges" TimeFare 4
    "EXTRA_TIME_FARE" -> charge "Extra Time Charges" ExtraTimeCharges 5
    "DEAD_KILOMETER_FARE" -> charge "Pickup Charges" PickupCharges 6
    "WAITING_CHARGES" -> charge waitingLabel waitingDescType 7
    "WAITING_OR_PICKUP_CHARGES" -> charge waitingLabel waitingDescType 8
    "PICKUP_CHARGES" -> charge "Pickup Charges" PickupCharges 9
    "PARKING_CHARGE" -> charge "Parking Charges" ParkingCharges 10
    -- ONDC v2.1.0 alias of PARKING_CHARGE (same field); dedupe collapses the pair
    "PARKING_CHARGES" -> charge "Parking Charges" ParkingCharges 10
    "TOLL_CHARGES" -> charge "Toll Charges" TollCharges 11
    "DRIVER_SELECTED_FARE" -> charge "Driver Addition**" DriverAddition 12
    "CUSTOMER_SELECTED_FARE" -> charge "Customer Addition*" CustomerAddition 13
    "CONGESTION_CHARGE" -> charge "Congestion Charges" CongestionCharge 14
    "EARLY_END_RIDE_PENALTY" -> charge "Early Ride End Charges^" EarlyEndRideCharges 15
    "SERVICE_CHARGE" -> charge "Service Charges" ServiceCharge 16
    "FIXED_GOVERNMENT_RATE" -> charge "Ride GST (5%)" RideGst 17
    "SGST" -> charge "Taxes (GST)" TaxesGst 18
    "PLATFORM_FEE" -> charge "Platform Fee" PlatformFee 19
    "CUSTOMER_CANCELLATION_DUES" -> charge "Cancellation Dues" CancellationDues 20
    "PET_CHARGES" -> charge "Pet Charges" PetCharges 22
    "PRIORITY_CHARGES" -> charge "Priority Charges" PriorityCharges 23
    "CANCELLATION_CHARGES" -> charge "Cancellation Charges" CancellationCharges 24
    "RIDE_CANCELLATION_CHARGES" -> charge "Ride Cancellation Charges" RideCancellationCharges 24
    "RIDE_CANCELLATION_TAX" -> charge "Ride Cancellation Tax" RideCancellationTax 25
    "BUSINESS_DISCOUNT" -> discount "Business Discount" BusinessDiscount 25
    "BUSINESS_DISCOUNT_PERCENTAGE" -> discount "Business Discount" BusinessDiscount 25
    "PERSONAL_DISCOUNT" -> discount "Personal Discount" PersonalDiscount 25
    "PERSONAL_DISCOUNT_PERCENTAGE" -> discount "Personal Discount" PersonalDiscount 25
    "GOVERNMENT_CHARGE" -> charge "Government Charges" GovernmentCharge 99
    "NIGHT_SHIFT_CHARGE" -> charge "Night Charge*" NightShiftCharge 99
    -- ONDC v2.1.0 alias of NIGHT_SHIFT_CHARGE (same field); dedupe collapses the pair
    "NIGHT_CHARGES" -> charge "Night Charge*" NightShiftCharge 99
    "PER_STOP_CHARGES" -> charge "Per Stop Charges" PerStopCharges 99
    "DRIVER_ALLOWANCE" -> charge "Driver Allowance" DriverAllowance 99
    "AIRPORT_CONVENIENCE_FEE" -> charge "Airport Convenience Fee" AirportConvenienceFee 99
    -- unknown tags are untyped by definition (title-cased passthrough; JL falls back to description)
    _ -> Just (titleCaseTag tag, Nothing, 99, AnyCharge)
  where
    charge label descType order = Just (label, Just descType, order, Charge)
    discount label descType order = Just (label, Just descType, order, Discount)
    (waitingLabel, waitingDescType) =
      if isSpecialZone then ("Pickup Charges", PickupCharges) else ("Waiting Charges**", WaitingCharges)

-- | Rate-card/config metadata stored as breakup rows (per-km rates, free-waiting minutes,
-- night-shift window times) plus TOTAL_FARE (a row would double-count) — never displayed,
-- as in the app. Explicit list so genuinely new tags still hit the passthrough.
droppedTags :: [Text]
droppedTags =
  [ "MIN_FARE",
    "PLANNED_PER_KM_CHARGE",
    "PLANNED_PER_KM_CHARGE_ROUND_TRIP",
    "PER_HOUR_CHARGE",
    "BASE_DISTANCE",
    "FREE_WAITING_TIME_IN_MINUTES",
    "NIGHT_SHIFT_START_TIME_IN_SECONDS",
    "NIGHT_SHIFT_END_TIME_IN_SECONDS",
    "WAITING_CHARGE_RATE_PER_MIN",
    "CONGESTION_CHARGE_PERCENTAGE",
    "DRIVER_MAX_EXTRA_FEE",
    "UNPLANNED_PER_KM_CHARGE",
    "PER_MINUTE_CHARGE",
    "TOTAL_FARE",
    "BUSINESS_DISCOUNT_DISCLAIMER"
  ]

titleCaseTag :: Text -> Text
titleCaseTag = T.unwords . map capitalize . T.splitOn "_"
  where
    capitalize w = case T.uncons w of
      Just (c, rest) -> T.cons (Char.toUpper c) (T.toLower rest)
      Nothing -> w
