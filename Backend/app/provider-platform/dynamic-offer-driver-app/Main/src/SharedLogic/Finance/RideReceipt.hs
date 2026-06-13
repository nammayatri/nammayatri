{-
  Finance.RideReceipt (BPP twin of rider-app SharedLogic.Finance.RideReceipt)

  Itemized ride-receipt assembly for the config-driven invoice PDF
  (TransporterConfig.invoiceConfig.useFareBreakupLineItems). BPP has no
  fare_breakup table — display line items derive from the booking's
  FareParameters via FareCalculator.mkFareParamsBreakups (the same display
  tags the BAP receives over Beckn), classified like the consumer app's
  getInvoiceFare so the dashboard PDF matches the in-app receipt. The
  finance_invoice row's clubbed lines are intentionally NOT the source.
-}
module SharedLogic.Finance.RideReceipt
  ( RideReceiptFields (..),
    mkItemizedRideLineItemsFromParams,
    mkRideReceiptFields,
    overrideIssuedToName,
    isSpecialZoneBooking,
  )
where

import qualified Data.Char as Char
import Data.List (nubBy, sortOn)
import qualified Data.Text as T
import qualified Data.Time as DT
import Domain.SharedLogic.RideDiscount (isProjectedFareParamTag)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), ItemType (..), LineItemDescription (..))
import qualified SharedLogic.FareCalculator as FC

data RowKind = Charge | Discount | AnyCharge

-- | FareParameters -> known charge tags render with the app's exact label when amount > 0; discount
-- tags render as negative adjustments; rate/config/projected tags are dropped;
-- unknown tags are title-cased and kept when non-zero. Sorted by the app's
-- invoiceOrder. Every item carries itemType (since parseLineItems errors on untyped).
mkItemizedRideLineItemsFromParams :: Bool -> DFP.FareParameters -> [InvoiceLineItem]
mkItemizedRideLineItemsFromParams isSpecialZone fareParams =
  dedupe . map snd . sortOn fst $ mapMaybe toRow (FC.mkFareParamsBreakups identity (,) fareParams)
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

-- | Special-zone (ride-OTP pickup) bookings relabel waiting charges as pickup charges,
-- exactly like the consumer app — BPP equivalent of the BAP bookingDetails check.
isSpecialZoneBooking :: DRB.Booking -> Bool
isSpecialZoneBooking booking = booking.tripCategory == DTC.OneWay DTC.OneWayRideOtp

-- | Rate-card/config metadata emitted as breakup items (per-km rates, free-waiting minutes,
-- night-shift window times) plus TOTAL_FARE (a row would double-count) — never displayed.
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

overrideIssuedToName :: Maybe Text -> FInvoice.Invoice -> FInvoice.Invoice
overrideIssuedToName mbName FInvoice.Invoice {..} = FInvoice.Invoice {issuedToName = mbName, ..}

-- | Ride-trip fields for the receipt template, pre-formatted in the merchant tz
-- (cleanJson would coerce raw ISO dates to %d.%m.%Y). Driver name and vehicle
-- number are caller-supplied (BPP ride carries neither directly).
data RideReceiptFields = RideReceiptFields
  { rideShortId :: Maybe Text,
    driverName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    pickupAddress :: Maybe Text,
    dropAddress :: Maybe Text,
    rideDate :: Maybe Text,
    rideStartTime :: Maybe Text,
    rideEndTime :: Maybe Text
  }

mkRideReceiptFields :: DT.TimeZone -> DRide.Ride -> DRB.Booking -> Text -> Text -> RideReceiptFields
mkRideReceiptFields tz ride booking driverName' vehicleNumber' =
  let startUtc = fromMaybe ride.createdAt ride.tripStartTime
   in RideReceiptFields
        { rideShortId = Just ride.shortId.getShortId,
          driverName = Just driverName',
          vehicleNumber = Just vehicleNumber',
          pickupAddress = Just (fullAddress booking.fromLocation),
          dropAddress = fullAddress <$> booking.toLocation,
          rideDate = Just (fmt "%a, %d %b, %Y" startUtc),
          rideStartTime = Just (fmt "%I:%M %p" startUtc),
          rideEndTime = fmt "%I:%M %p" <$> ride.tripEndTime
        }
  where
    fmt pattern' = T.pack . DT.formatTime DT.defaultTimeLocale pattern' . DT.utcToLocalTime tz

fullAddress :: DLoc.Location -> Text
fullAddress loc =
  let a = loc.address
      parts = catMaybes [a.door, a.building, a.street, a.area, a.city, a.state, a.country]
   in -- parts-join first, matching the BAP twin so both surfaces print the same text;
      -- stored fullAddress only as fallback when no parts exist
      if null parts then fromMaybe "" a.fullAddress else T.intercalate ", " parts
