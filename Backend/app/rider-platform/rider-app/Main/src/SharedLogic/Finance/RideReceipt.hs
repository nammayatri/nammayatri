{-
  Finance.RideReceipt

  Itemized ride-receipt assembly for the config-driven invoice PDF
  (RiderInvoiceConfig.useFareBreakupLineItems). Line items are built from the
  ride's fareBreakup display tags — a faithful port of the consumer app's
  getInvoiceFare (ny-react-native fareEntityHelper.ts) so the backend PDF
  matches the in-app receipt. The finance_invoice row is intentionally NOT the
  line-item source here: its lines are a clubbed tax summary (one "Ride Fare"),
  not the per-component breakdown the receipt shows.
-}
module SharedLogic.Finance.RideReceipt
  ( RideReceiptFields (..),
    mkItemizedRideLineItems,
    mkOfferDiscountItem,
    synthFinanceInvoice,
    overrideIssuedToName,
    mkRideReceiptFields,
    riderDisplayName,
    isSpecialZoneBooking,
  )
where

import qualified Data.Aeson as A
import qualified Data.Char as Char
import Data.List (nubBy, sortOn)
import qualified Data.Text as T
import qualified Data.Time as DT
import Domain.SharedLogic.RideDiscount (isProjectedFareParamTag)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id (..))
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), ItemType (..), LineItemDescription (..))

data RowKind = Charge | Discount | AnyCharge

-- | fareBreakup display tags -> receipt rows, mirroring getInvoiceFare:
-- known charge tags render with the app's exact label when amount > 0;
-- discount tags render as negative adjustments; rate/config/estimate tags are
-- dropped; unknown tags are title-cased and kept when non-zero (getAnyFare).
-- Sorted by the app's invoiceOrder. Every item carries itemType (parseLineItems
-- errors on untyped items).
mkItemizedRideLineItems :: Bool -> [DFareBreakup.FareBreakup] -> [InvoiceLineItem]
mkItemizedRideLineItems isSpecialZone breakups =
  dedupe . map snd . sortOn fst $ mapMaybe toRow breakups
  where
    -- FareCalculator emits ONDC v2.1.0 duplicate tags from the same source field
    -- (PARKING_CHARGE+PARKING_CHARGES, NIGHT_SHIFT_CHARGE+NIGHT_CHARGES,
    -- WAITING_OR_PICKUP_CHARGES+WAITING_CHARGES); without this collapse the pair
    -- renders twice and inflates finalAmount past what the customer paid.
    dedupe = nubBy (\a b -> a.description == b.description && a.lineTotal == b.lineTotal)
    toRow fb =
      let amt = fb.amount.amount
       in classifyTag isSpecialZone fb.description >>= \(label, descType, order, kind) ->
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

-- | Offer discount as an explicit receipt row (the offer's charge-reduction
-- amount, NOT cashback), so the fare list sums to the amount actually paid —
-- for rides whose fareBreakup carries no OFFER_DISCOUNT row.
mkOfferDiscountItem :: Maybe Text -> HighPrecMoney -> [InvoiceLineItem]
mkOfferDiscountItem mbTitle discountAmount
  | discountAmount > 0 =
    [ InvoiceLineItem
        { description = fromMaybe "Offer Discount" mbTitle,
          descriptionType = Nothing,
          quantity = 1,
          unitPrice = negate discountAmount,
          lineTotal = negate discountAmount,
          isExternalCharge = False,
          groupId = Nothing,
          itemType = Just Adjustment
        }
    ]
  | otherwise = []

-- | Backward compat: 'buildInvoiceContext' requires an Invoice value, but rides predating
-- finance_invoice rows have none — so synthesize one IN MEMORY (never persisted) from
-- ride/booking. Only the fields the receipt template renders carry real values
-- (invoiceNumber = ride shortId, ids, currency, totals from the itemized rows, issuedAt,
-- issuedTo*); the Nothings (irn, supplier*, gstin, QR, payment/period metadata) belong to
-- invoices of merchants with the flag useFareBreakupLineItems False, which never reaches this path.
synthFinanceInvoice :: InvoiceType -> DRide.Ride -> DRB.Booking -> Maybe Text -> [InvoiceLineItem] -> FInvoice.Invoice
synthFinanceInvoice invoiceType ride booking mbRiderName items =
  let issuedAt = fromMaybe ride.createdAt ride.rideStartTime
      totalAmount = sum $ map (.lineTotal) items
   in FInvoice.Invoice
        { id = Id ride.id.getId,
          invoiceNumber = ride.shortId.getShortId,
          invoiceType,
          entityReferenceId = Nothing,
          referenceInvoiceNumber = Nothing,
          issuedToType = RIDER,
          issuedToId = booking.riderId.getId,
          issuedToName = mbRiderName,
          issuedToAddress = Nothing,
          issuedByType = "BUYER",
          issuedById = booking.merchantId.getId,
          issuedByName = Nothing,
          issuedByAddress = Nothing,
          supplierName = Nothing,
          supplierAddress = Nothing,
          supplierGSTIN = Nothing,
          supplierTaxNo = Nothing,
          supplierId = Nothing,
          merchantGstin = Nothing,
          referenceId = Just ride.id.getId,
          lineItems = A.toJSON items,
          subtotal = totalAmount,
          taxBreakdown = Nothing,
          totalAmount,
          currency = booking.estimatedFare.currency,
          status = FInvoice.Issued,
          issuedAt,
          dueAt = Nothing,
          periodStart = Nothing,
          periodEnd = Nothing,
          merchantId = booking.merchantId.getId,
          merchantOperatingCityId = booking.merchantOperatingCityId.getId,
          createdAt = issuedAt,
          updatedAt = issuedAt,
          createdBy = Nothing,
          createdById = Nothing,
          updatedBy = Nothing,
          updatedById = Nothing,
          irn = Nothing,
          signedQRCode = Nothing,
          paymentMode = Nothing
        }

overrideIssuedToName :: Maybe Text -> FInvoice.Invoice -> FInvoice.Invoice
overrideIssuedToName mbName FInvoice.Invoice {..} = FInvoice.Invoice {issuedToName = mbName, ..}

-- | Ride-trip fields for the receipt template, pre-formatted in the merchant tz
-- (cleanJson would coerce raw ISO dates to %d.%m.%Y).
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

mkRideReceiptFields :: DT.TimeZone -> DRide.Ride -> DRB.Booking -> RideReceiptFields
mkRideReceiptFields tz ride booking =
  let startUtc = fromMaybe ride.createdAt ride.rideStartTime
   in RideReceiptFields
        { rideShortId = Just ride.shortId.getShortId,
          driverName = Just ride.driverName,
          vehicleNumber = Just ride.vehicleNumber,
          pickupAddress = Just (fullAddress booking.fromLocation),
          dropAddress = fullAddress <$> bookingDropLocation booking,
          rideDate = Just (fmt "%a, %d %b, %Y" startUtc),
          rideStartTime = Just (fmt "%I:%M %p" startUtc),
          rideEndTime = fmt "%I:%M %p" <$> ride.rideEndTime
        }
  where
    fmt pattern' = T.pack . DT.formatTime DT.defaultTimeLocale pattern' . DT.utcToLocalTime tz

bookingDropLocation :: DRB.Booking -> Maybe DLoc.Location
bookingDropLocation booking = case booking.bookingDetails of
  DRB.OneWayDetails d -> Just d.toLocation
  DRB.RentalDetails d -> d.stopLocation
  DRB.DriverOfferDetails d -> Just d.toLocation
  DRB.OneWaySpecialZoneDetails d -> Just d.toLocation
  DRB.InterCityDetails d -> Just d.toLocation
  DRB.AmbulanceDetails d -> Just d.toLocation
  DRB.DeliveryDetails d -> Just d.toLocation
  DRB.MeterRideDetails d -> d.toLocation

fullAddress :: DLoc.Location -> Text
fullAddress loc =
  let a = loc.address
      parts = catMaybes [a.door, a.building, a.street, a.area, a.city, a.state, a.country]
   in if null parts then fromMaybe "" a.area else T.intercalate ", " parts

riderDisplayName :: DP.Person -> Maybe Text
riderDisplayName person = case (person.firstName, person.lastName) of
  (Nothing, Nothing) -> Nothing
  (mbFirst, mbLast) -> Just $ T.strip (fromMaybe "" mbFirst <> maybe "" (" " <>) mbLast)

isSpecialZoneBooking :: DRB.Booking -> Bool
isSpecialZoneBooking booking = case booking.bookingDetails of
  DRB.OneWaySpecialZoneDetails _ -> True
  _ -> False
