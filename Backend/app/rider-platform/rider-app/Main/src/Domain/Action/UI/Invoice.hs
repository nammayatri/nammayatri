{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Invoice where

import qualified API.Types.UI.Invoice as DTInvoice
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.Id
import Servant
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.FareBreakup as CHFB
import qualified Storage.Clickhouse.Location as CHL
import qualified Storage.Clickhouse.Ride as CHR
import Tools.Auth

getInvoice :: (Id DP.Person, Id DM.Merchant) -> UTCTime -> UTCTime -> Flow [DTInvoice.InvoiceRes]
getInvoice (personId, merchantId) from to = do
  bookings <- CHB.findAllCompletedRiderBookingsByMerchantInRange merchantId personId from to
  invoices <- mapM makeInvoiceResponse bookings
  return $ catMaybes invoices
  where
    makeInvoiceResponse booking = do
      mbRide <- CHR.findRideByBookingId booking.id
      case mbRide of
        Just ride -> do
          mbBaseFare <- CHFB.findFareBreakupByBookingIdAndDescription booking.id "BASE_FARE"
          mbPickupCharge <- CHFB.findFareBreakupByBookingIdAndDescription booking.id "DEAD_KILOMETER_FARE"
          mbDriverExtraFeeBounds <- CHFB.findFareBreakupByBookingIdAndDescription booking.id "DRIVER_SELECTED_FARE"
          mbCancellationDues <- CHFB.findFareBreakupByBookingIdAndDescription booking.id "CUSTOMER_CANCELLATION_DUES"
          mbSource <- join <$> mapM CHL.findLocationById booking.fromLocationId
          mbDestination <- join <$> mapM CHL.findLocationById booking.toLocationId
          let baseFare = maybe 0.0 (\baseFare' -> fromMaybe 0.0 baseFare'.amount) mbBaseFare
              pickupCharge = maybe 0.0 (\pickupCharge' -> fromMaybe 0.0 pickupCharge'.amount) mbPickupCharge
              driverExtraFeeBounds = maybe 0.0 (\driverExtraFeeBounds' -> fromMaybe 0.0 driverExtraFeeBounds'.amount) mbDriverExtraFeeBounds
              cancellationDues = maybe 0.0 (\cancellationDues' -> fromMaybe 0.0 cancellationDues'.amount) mbCancellationDues
          return $
            Just $
              DTInvoice.InvoiceRes
                { date = booking.createdAt,
                  destination = maybe "" (\destination -> fromMaybe "" destination.fullAddress) mbDestination,
                  driverName = fromMaybe "" ride.driverName,
                  faresList =
                    [ DTInvoice.FareBreakup {price = baseFare, title = "BASE_FARE"},
                      DTInvoice.FareBreakup {price = pickupCharge, title = "DEAD_KILOMETER_FARE"},
                      DTInvoice.FareBreakup {price = driverExtraFeeBounds, title = "DRIVER_SELECTED_FARE"},
                      DTInvoice.FareBreakup {price = cancellationDues, title = "CUSTOMER_CANCELLATION_DUES"}
                    ],
                  rideEndTime = fromMaybe ride.updatedAt ride.rideEndTime,
                  rideStartTime = fromMaybe ride.createdAt ride.rideStartTime,
                  shortRideId = ride.shortId.getShortId,
                  source = maybe "" (\source -> fromMaybe "" source.fullAddress) mbSource,
                  totalAmount = fromMaybe (baseFare + pickupCharge + driverExtraFeeBounds + cancellationDues) ride.totalFare,
                  vehicleNumber = fromMaybe "" ride.vehicleNumber
                }
        Nothing -> return Nothing
