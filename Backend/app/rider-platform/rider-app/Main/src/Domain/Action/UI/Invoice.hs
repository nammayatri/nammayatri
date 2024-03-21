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
import Kernel.Utils.Common
import Servant
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.FareBreakup as CHFB
import qualified Storage.Clickhouse.Location as CHL
import qualified Storage.Clickhouse.Ride as CHR
import Tools.Auth
import Tools.Error

getInvoice :: (Maybe (Id DP.Person), Id DM.Merchant) -> UTCTime -> UTCTime -> Flow [DTInvoice.InvoiceRes]
getInvoice (mbPersonId, merchantId) from to = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  bookings <- CHB.findAllCompletedRiderBookingsByMerchantInRange merchantId personId from to
  invoices <- mapM makeInvoiceResponse bookings
  return $ catMaybes invoices
  where
    makeInvoiceResponse booking = do
      mbRide <- CHR.findRideByBookingId booking.id
      case mbRide of
        Just ride -> do
          let breakupItems =
                [ ("BASE_FARE", "Base Fare"),
                  ("CUSTOMER_SELECTED_FARE", "Customer Selected Fare"),
                  ("DEAD_KILOMETER_FARE", "Dead Kilometer Fare"),
                  ("DISTANCE_FARE", "Distance Fare"),
                  ("DRIVER_SELECTED_FARE", "Driver Selected Fare"),
                  ("EXTRA_TIME_FARE", "Extra Time Fare"),
                  ("FIXED_GOVERNMENT_RATE", "Fixed Government Fare"),
                  ("NIGHT_SHIFT_CHARGE", "Night Shift Charge"),
                  ("PLATFORM_FEE", "Platform Fee"),
                  ("CGST", "CGST"),
                  ("SGST", "SGST"),
                  ("SERVICE_CHARGE", "Service Charge"),
                  ("TIME_BASED_FARE", "Time Based Fare"),
                  ("DIST_BASED_FARE", "Distance Based Fare"),
                  ("WAITING_OR_PICKUP_CHARGES", "Wating Charge")
                ]
          fareBreakups <- mapM (getFareBreakup booking.id) breakupItems
          mbSource <- join <$> mapM CHL.findLocationById booking.fromLocationId
          mbDestination <- join <$> mapM CHL.findLocationById booking.toLocationId
          return $
            Just $
              DTInvoice.InvoiceRes
                { date = booking.createdAt,
                  destination = maybe notAvailableText (\destination -> fromMaybe notAvailableText destination.fullAddress) mbDestination,
                  driverName = fromMaybe notAvailableText ride.driverName,
                  faresList = catMaybes fareBreakups,
                  rideEndTime = fromMaybe ride.updatedAt ride.rideEndTime,
                  rideStartTime = fromMaybe ride.createdAt ride.rideStartTime,
                  shortRideId = ride.shortId.getShortId,
                  source = maybe notAvailableText (\source -> fromMaybe notAvailableText source.fullAddress) mbSource,
                  totalAmount = maybe notAvailableText show ride.totalFare,
                  vehicleNumber = fromMaybe notAvailableText ride.vehicleNumber,
                  chargeableDistance = ride.chargeableDistance
                }
        Nothing -> return Nothing
    getFareBreakup bookingId (tag, title) = do
      fareBreakup <- CHFB.findFareBreakupByBookingIdAndDescription bookingId tag
      case fareBreakup of
        Just breakup -> return . Just $ DTInvoice.FareBreakup {price = maybe notAvailableText show breakup.amount, title}
        Nothing -> return Nothing
    notAvailableText = "N/A"
