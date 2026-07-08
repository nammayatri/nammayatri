{-
  Finance.RideReceipt

  Itemized ride-receipt assembly for the config-driven invoice PDF
  (Merchant.useFareBreakupLineItems). Line items are built from the
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
import qualified Data.Text as T
import qualified Data.Time as DT
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
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), ItemType (..))
import Lib.Finance.Invoice.LineItems (mkLineItemsFromTaggedAmounts)

-- | BAP itemized receipt: reduce the ride's fareBreakup rows to (tag, amount)
-- pairs and hand them to the shared classifier.
mkItemizedRideLineItems :: Bool -> [DFareBreakup.FareBreakup] -> [InvoiceLineItem]
mkItemizedRideLineItems isSpecialZone breakups =
  mkLineItemsFromTaggedAmounts isSpecialZone (map (\fb -> (fb.description, fb.amount.amount)) breakups)

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
