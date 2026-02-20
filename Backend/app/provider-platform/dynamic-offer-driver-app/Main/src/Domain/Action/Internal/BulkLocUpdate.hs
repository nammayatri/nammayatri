{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BulkLocUpdate where

import qualified Data.List.NonEmpty as NE
import Data.OpenApi (ToSchema)
import Domain.Action.UI.Ride.EndRide.Internal (getRouteInfoWithShortestDuration)
import qualified Domain.Types as DC
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
import qualified SharedLogic.CallBAP as CallBAP
import SharedLogic.Ride as SRide
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as TM
import Tools.Utils (isDropInsideThreshold)

data BulkLocUpdateReq = BulkLocUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

bulkLocUpdate :: BulkLocUpdateReq -> Flow APISuccess
bulkLocUpdate req = do
  let driverId = req.driverId
      rideId = req.rideId
      loc = req.loc
  logDebug $ "BulkLocUpdate = " <> show rideId <> " " <> show driverId <> " " <> show loc
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  transportConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") $ ride.merchantId
  let minUpdatesToTriggerSnapToRoad = getMinLocUpdateCountForDistanceCalculation transportConfig ride.tripCategory
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId ride.merchantOperatingCityId (Just rideId) False (Just minUpdatesToTriggerSnapToRoad)
  rectificationServiceConfig <-
    if DC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
      else pure Nothing
  let isTollApplicable = DC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory
  let passedThroughDrop = any (isDropInsideThreshold booking transportConfig) loc
  logDebug $ "Did we passed through drop yet in bulkLocation  " <> show passedThroughDrop <> " and points: " <> show loc
  _ <- addIntermediateRoutePoints defaultRideInterpolationHandler rectificationServiceConfig isTollApplicable transportConfig.enableTollCrossedNotifications rideId driverId passedThroughDrop (booking.tripCategory == DC.OneWay DC.MeterRide) loc

  let buffertime' = getArrivalTimeBufferOfVehicle transportConfig.arrivalTimeBufferOfVehicle booking.vehicleServiceTier
  case (buffertime', ride.estimatedEndTimeRange, ride.toLocation) of
    (Just buffertime, Just endTimeRange, Just toLocation) | ride.status == DRide.INPROGRESS -> do
      now <- getCurrentTime
      when (now > addUTCTime (secondsToNominalDiffTime (div buffertime 2)) endTimeRange.start && not passedThroughDrop) $
        fork "update estimated end time" $ do
          logDebug $ "Updating estimated end time for ride " <> show rideId
          let currentLatLong = NE.last loc
              dropLatLong = TM.LatLong {lat = toLocation.lat, lon = toLocation.lon}
          routeResponse <-
            TM.getRoutes merchantId booking.merchantOperatingCityId (Just rideId.getId) $
              TM.GetRoutesReq
                { waypoints = NE.fromList [currentLatLong, dropLatLong],
                  mode = Just TM.CAR,
                  calcPoints = True
                }
          shortestRoute <- getRouteInfoWithShortestDuration routeResponse & fromMaybeM (InternalError "No route found for latlongs")
          (duration :: Seconds) <- shortestRoute.duration & fromMaybeM (InternalError "No duration found for new route")
          let newEstimatedEndTimeRange = SRide.calculateEstimatedEndTimeRange now duration transportConfig.arrivalTimeBufferOfVehicle booking.vehicleServiceTier
          let updatedRide = ride {DRide.estimatedEndTimeRange = newEstimatedEndTimeRange}
          QRide.updateEstimatedEndTimeRange newEstimatedEndTimeRange rideId
          CallBAP.sendRideEstimatedEndTimeRangeUpdateToBAP booking updatedRide
    _ -> pure ()
  pure Success
  where
    getMinLocUpdateCountForDistanceCalculation transporterConfig tripCategory =
      case tripCategory of
        (DC.OneWay DC.MeterRide) -> transporterConfig.meterRideBulkLocUpdateBatchSize
        _ -> transporterConfig.normalRideBulkLocUpdateBatchSize
