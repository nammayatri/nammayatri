module Domain.Action.Dashboard.Booking
  ( stuckBookingsCancel,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide

---------------------------------------------------------------------

-- cancel all stuck bookings/rides:
--   bookings, when status is NEW for more than 6 hours
--   bookings and rides, when ride status is NEW for more than 6 hours

stuckBookingsCancel ::
  ShortId DM.Merchant ->
  Common.StuckBookingsCancelReq ->
  Flow Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let reqBookingIds = cast @Common.Booking @DBooking.Booking <$> req.bookingIds
  now <- getCurrentTime
  stuckBookingIds <- Esq.runInReplica $ QBooking.findStuckBookings merchant.id reqBookingIds now
  stuckRideItems <- Esq.runInReplica $ QRide.findStuckRideItems merchant.id reqBookingIds now
  let bcReasons = mkBookingCancellationReason Common.bookingStuckCode Nothing <$> stuckBookingIds
  let bcReasonsWithRides = (\item -> mkBookingCancellationReason Common.rideStuckCode (Just item.rideId) item.bookingId) <$> stuckRideItems
  let allStuckBookingIds = stuckBookingIds <> (stuckRideItems <&> (.bookingId))
  let stuckPersonIds = stuckRideItems <&> (.riderId)
  Esq.runTransaction $ do
    QRide.cancelRides (stuckRideItems <&> (.rideId)) now
    QBooking.cancelBookings allStuckBookingIds now
    for_ (bcReasons <> bcReasonsWithRides) QBCR.upsert
    QPFS.updateToIdleMultiple stuckPersonIds now
  logTagInfo "dashboard -> stuckBookingsCancel: " $ show allStuckBookingIds
  pure $ mkStuckBookingsCancelRes stuckBookingIds stuckRideItems

mkBookingCancellationReason :: Common.CancellationReasonCode -> Maybe (Id DRide.Ride) -> Id DBooking.Booking -> DBCR.BookingCancellationReason
mkBookingCancellationReason reasonCode mbRideId bookingId = do
  DBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      source = DBCR.ByMerchant,
      reasonCode = Just $ coerce @Common.CancellationReasonCode @DCR.CancellationReasonCode reasonCode,
      reasonStage = Nothing,
      additionalInfo = Nothing
    }

mkStuckBookingsCancelRes :: [Id DBooking.Booking] -> [QRide.StuckRideItem] -> Common.StuckBookingsCancelRes
mkStuckBookingsCancelRes stuckBookingIds stuckRideItems = do
  let bookingItems = (stuckBookingIds <&>) $ \bookingId -> do
        Common.StuckBookingItem
          { rideId = Nothing,
            bookingId = cast @DBooking.Booking @Common.Booking bookingId
          }
  let rideItems = (stuckRideItems <&>) $ \QRide.StuckRideItem {..} -> do
        Common.StuckBookingItem
          { rideId = Just $ cast @DRide.Ride @Common.Ride rideId,
            bookingId = cast @DBooking.Booking @Common.Booking bookingId
          }
  Common.StuckBookingsCancelRes
    { cancelledBookings = rideItems <> bookingItems
    }
