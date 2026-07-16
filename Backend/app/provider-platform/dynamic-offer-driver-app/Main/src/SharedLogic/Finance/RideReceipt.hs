{-
  Finance.RideReceipt (BPP twin of rider-app SharedLogic.Finance.RideReceipt)

  Itemized ride-receipt assembly for the config-driven invoice PDF
  (Merchant.useFareBreakupLineItems). BPP has no
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

import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem)
import Lib.Finance.Invoice.LineItems (mkLineItemsFromTaggedAmounts)
import qualified SharedLogic.FareCalculator as FC

-- | BPP itemized receipt: derive (tag, amount) pairs from the booking's
-- FareParameters (the same display tags the BAP receives over Beckn) and hand
-- them to the shared classifier. BPP has no fare_breakup table.
mkItemizedRideLineItemsFromParams :: Bool -> DFP.FareParameters -> [InvoiceLineItem]
mkItemizedRideLineItemsFromParams isSpecialZone fareParams =
  mkLineItemsFromTaggedAmounts isSpecialZone (FC.mkFareParamsBreakups identity (,) fareParams)

-- | Special-zone (ride-OTP pickup) bookings relabel waiting charges as pickup charges,
-- exactly like the consumer app — BPP equivalent of the BAP bookingDetails check.
isSpecialZoneBooking :: DRB.Booking -> Bool
isSpecialZoneBooking booking = booking.tripCategory == DTC.OneWay DTC.OneWayRideOtp

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
