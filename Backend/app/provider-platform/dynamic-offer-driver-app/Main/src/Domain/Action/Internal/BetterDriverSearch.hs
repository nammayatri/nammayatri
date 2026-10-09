{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BetterDriverSearch
  ( BetterDriverSearchReq (..),
    betterDriverSearch,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps (HasCoordinates (getCoordinates))
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.BetterDriverSearch as SBDS
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSF
import Storage.CachedQueries.Merchant as QMerchant
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

newtype BetterDriverSearchReq = BetterDriverSearchReq
  { bookingId :: Id DRB.Booking
  }
  deriving (Generic, ToJSON, FromJSON)

betterDriverSearch :: BetterDriverSearchReq -> Flow APISuccess
betterDriverSearch req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  ride <- QRide.findActiveByRBId booking.id >>= fromMaybeM (RideNotFound $ "no active ride for booking: " <> booking.id.getId)
  unless (ride.status == DRide.UPCOMING || ride.status == DRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  -- One-attempt-per-booking: block if a stand-by search is already running for this
  -- booking, so a double-tap/retry (frontend is only a soft deterrent, not a guarantee)
  -- can never result in two concurrent stand-by searches for the same booking.
  mbActiveBetterDriverSearchTry <- QST.findActiveBetterDriverSearchByBookingId booking.id.getId
  whenJust mbActiveBetterDriverSearchTry $ \_ ->
    throwError (InvalidRequest "A better-driver search is already running for this booking")
  merchant <- QMerchant.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  whenJust transporterConfig.betterDriverProximityAbortRadiusMeters $ \abortRadius -> do
    -- ride.status is NEW/UPCOMING here (checked above) - pings for this driver are
    -- landing in LTS's on_pickup bucket, not on_ride, so this must read that bucket.
    driverLocationResp <- LTSF.pickupDriverLocation ride.id merchant.id ride.driverId
    whenJust (lastMaybe driverLocationResp.loc) $ \latest -> do
      let pickupLoc = getCoordinates booking.fromLocation
          distance = distanceBetweenInMeters (LatLong latest.lat latest.lon) pickupLoc
      when (distance < abortRadius) $ throwError (InvalidRequest "Driver is already close to pickup; not eligible for a better-driver search")
  SBDS.startBetterDriverSearch merchant booking ride
  pure Success
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)
