{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BulkLocPickupUpdate where

import qualified Data.List.NonEmpty as NE
import Data.OpenApi (ToSchema)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSF
import qualified SharedLogic.External.LocationTrackingService.Types as LTS
import qualified Storage.Queries.Ride as QRide

data BulkLocPickupUpdateReq = BulkLocPickupUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LTS.LocationUpdate
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

bulkLocPickupUpdate :: BulkLocPickupUpdateReq -> Flow APISuccess
bulkLocPickupUpdate req = do
  let driverId = req.driverId
      rideId = req.rideId
      loc = req.loc
      accLoc = NE.filter (\w -> maybe True (< 50) w.acc) loc
  logDebug $ "BulkLocPickupUpdate = " <> show rideId <> " " <> show driverId <> " " <> show loc
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") ride.merchantId
  pickupInterpolationHandler <- LocUpd.buildRidePickupInterpolationHandler merchantId ride.merchantOperatingCityId (Just rideId) False Nothing
  currentTime <- getCurrentTime
  let nowTs = floor $ utcTimeToPOSIXSeconds currentTime
      mkWaypointWithTime w = (LatLong w.lat w.lon, fromMaybe nowTs w.ts)
      allWaypointsWithTime = fmap mkWaypointWithTime loc
      accWaypointsWithTime = map mkWaypointWithTime accLoc
      pickupDriverId = LocUpd.pickupLocationUpdatesDriverId driverId
  addIntermediatePickupPoints pickupInterpolationHandler rideId pickupDriverId accWaypointsWithTime allWaypointsWithTime
  pure Success

-- | Called at ride start: drains any pickup points still buffered in LTS,
-- runs the final snap-to-road over the whole accumulated pickup points.
finalizePickupDistanceOnRideStart ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  Id DP.Person ->
  Flow ()
finalizePickupDistanceOnRideStart merchantId merchantOpCityId rideId driverId = do
  pickupInterpolationHandler <- LocUpd.buildRidePickupInterpolationHandler merchantId merchantOpCityId (Just rideId) True Nothing
  let pickupDriverId = LocUpd.pickupLocationUpdatesDriverId driverId
  remaining <- LTSF.pickupDriverLocation rideId merchantId driverId
  currentTime <- getCurrentTime
  let nowTs = floor $ utcTimeToPOSIXSeconds currentTime
      mkWaypointWithTime w = (LatLong w.lat w.lon, fromMaybe nowTs w.ts)
      accRemaining = filter (\w -> maybe True (< 50) w.acc) remaining.loc
      remainingWaypoints = map mkWaypointWithTime accRemaining
  -- If LTS has no remaining points, re-feed the last buffered point (0 added
  -- distance) purely to trigger the final drain of the accumulated buffer.
  mbFinalWaypoints <- case nonEmpty remainingWaypoints of
    Just neWps -> pure (Just neWps)
    Nothing -> do
      buffered <- pickupInterpolationHandler.getAllWaypoints pickupDriverId
      pure $ (\neBuffered -> (NE.last neBuffered, nowTs) :| []) <$> nonEmpty buffered
  whenJust mbFinalWaypoints $ \finalWaypoints ->
    finalPickupDistanceCalculation pickupInterpolationHandler rideId pickupDriverId (NE.toList finalWaypoints) finalWaypoints
  pickupInterpolationHandler.clearLocationUpdates pickupDriverId
  pickupInterpolationHandler.clearInterpolatedPoints pickupDriverId
