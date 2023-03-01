{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Booking
  ( stuckBookingsCancel,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverLocation as SDrLoc
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Ride as SRide
import qualified Storage.CachedQueries.DriverInformation as CQDrInfo
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverLocation as QDrLoc
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
  stuckBookingIds <- Esq.runInReplica $ QBooking.findStuckBookings merchant.id reqBookingIds now (Proxy @Flow)
  stuckRideItems <- Esq.runInReplica $ QRide.findStuckRideItems merchant.id reqBookingIds now (Proxy @Flow)
  let bcReasons = mkBookingCancellationReason Common.bookingStuckCode Nothing <$> stuckBookingIds
  let bcReasonsWithRides = (\item -> mkBookingCancellationReason Common.rideStuckCode (Just item.rideId) item.bookingId) <$> stuckRideItems
  let allStuckBookingIds = stuckBookingIds <> (stuckRideItems <&> (.bookingId))
  let stuckPersonIds = stuckRideItems <&> (.driverId)
  let stuckDriverIds = cast @DP.Person @DP.Driver <$> stuckPersonIds
  -- drivers going out of ride, update location from redis to db
  driverLocations <- catMaybes <$> traverse SDrLoc.findById stuckPersonIds
  Esq.runTransaction $ do
    QRide.updateStatusByIds @Flow (stuckRideItems <&> (.rideId)) DRide.CANCELLED
    QBooking.cancelBookings allStuckBookingIds now
    for_ (bcReasons <> bcReasonsWithRides) QBCR.upsert
    for_ driverLocations $ \location -> do
      let latLong = LatLong location.lat location.lon
      QDrLoc.upsertGpsCoord location.driverId latLong location.coordinatesCalculatedAt
    CQDrInfo.updateNotOnRideMultiple stuckDriverIds
    for_ stuckRideItems $ \item -> do
      if item.driverActive
        then QDFS.updateStatus item.driverId DDFS.ACTIVE
        else QDFS.updateStatus item.driverId DDFS.IDLE
  for_ stuckDriverIds CQDrInfo.clearDriverInfoCache
  for_ stuckPersonIds SRide.clearCache
  logTagInfo "dashboard -> stuckBookingsCancel: " $ show allStuckBookingIds
  pure $ mkStuckBookingsCancelRes stuckBookingIds stuckRideItems

mkBookingCancellationReason :: Common.CancellationReasonCode -> Maybe (Id DRide.Ride) -> Id DBooking.Booking -> DBCR.BookingCancellationReason
mkBookingCancellationReason reasonCode mbRideId bookingId = do
  DBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      source = DBCR.ByMerchant,
      reasonCode = Just $ coerce @Common.CancellationReasonCode @DCR.CancellationReasonCode reasonCode,
      driverId = Nothing,
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
