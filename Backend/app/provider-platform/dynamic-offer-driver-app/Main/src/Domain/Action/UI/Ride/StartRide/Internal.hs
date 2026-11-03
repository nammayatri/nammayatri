{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.StartRide.Internal (startRideTransaction, mkInProgressRideLtsInfo) where

import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant as Dmerch
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.Vehicle as Fields
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, HasShortDurationRetryCfg, fromMaybeM, logTagInfo)
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.DriverPool.DriverAnalyticsEvent as DAE
import SharedLogic.DriverPool.DriverPoolStateEvent (DriverPoolStateEnv)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import Storage.Beam.IncentiveJourney ()
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Event
import qualified Tools.Fields as Fields

startRideTransaction ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r,
    LT.HasLocationService m r,
    BeamFlow m r,
    Metrics.CoreMetrics m,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "minDistanceBetweenTwoPoints" r Int,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    HasShortDurationRetryCfg r c,
    Redis.HedisLTSFlowEnv r,
    DriverPoolStateEnv r
  ) =>
  Id SP.Person ->
  SRide.Ride ->
  Id SRB.Booking ->
  LatLong ->
  Id Dmerch.Merchant ->
  Maybe SRide.OdometerReading ->
  DTConf.TransporterConfig ->
  DDI.DriverInformation ->
  m ()
startRideTransaction driverId ride bookingId firstPoint merchantId odometer transporterConfig driverInfo = do
  triggerRideStartEvent RideEventData {ride = ride{SRide.status = SRide.INPROGRESS}, personId = driverId, merchantId = merchantId}
  -- LTS doesn't reset its pickup notification status at start, so without the flag it would report DRIVER_REACHED at the start point.
  rideInfo <-
    if DTC.isSharedRouteTrip ride.tripCategory
      then mkSharedRunLtsInfo ride
      else mkInProgressRideLtsInfo ride firstPoint transporterConfig.enableManualArrival
  void $ LF.rideStart ride.id firstPoint.lat firstPoint.lon merchantId driverId (Just rideInfo)
  QRide.updateStartTimeAndLoc ride.id firstPoint
  whenJust odometer $ \odometerReading -> QRide.updateStartOdometerReading ride.id odometerReading
  logTagInfo "startRide" ("Updating driver_flow_status to ON_RIDE for DriverId " <> getId driverId)
  DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig driverInfo.active Nothing DDFS.ON_RIDE driverInfo (Just True) Nothing
  QRide.updateStatus ride.id SRide.INPROGRESS
  DAE.publishRideStarted ride
  QBE.logRideCommencedEvent (cast driverId) bookingId ride.id ride.distanceUnit

-- | A shared-route run is a bus to the location service: pings carry the route and the plate, and the bus pipeline writes
-- stop ETAs under @route:{routeCode}@ for the rider platform's tracking, exactly as a shared cab's run does.
mkSharedRunLtsInfo :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SRide.Ride -> m LT.RideInfo
mkSharedRunLtsInfo ride = do
  routeCode <- ride.routeCode & fromMaybeM (InternalError $ "shared run " <> ride.id.getId <> " has no route code")
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  destination <- ride.toLocation & fromMaybeM (InternalError $ "shared run " <> ride.id.getId <> " has no last stop")
  pure $
    LT.Bus
      LT.BusRideInfo
        { routeCode,
          busNumber = vehicle.registrationNo,
          source = LatLong ride.fromLocation.lat ride.fromLocation.lon,
          destination = LatLong destination.lat destination.lon,
          routeLongName = Nothing,
          driverName = Nothing,
          groupId = Nothing
        }

mkInProgressRideLtsInfo :: (MonadReader r m, HasField "minDistanceBetweenTwoPoints" r Int) => SRide.Ride -> LatLong -> Maybe Bool -> m LT.RideInfo
mkInProgressRideLtsInfo ride startPoint isManualArrival = do
  minDistanceBetweenTwoPoints <- bool (pure Nothing) (Just <$> asks (.minDistanceBetweenTwoPoints)) (ride.tripCategory == DTC.OneWay DTC.MeterRide)
  pure $
    LT.Car
      LT.CarRideInfo
        { pickupLocation = startPoint,
          minDistanceBetweenTwoPoints,
          rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) ride.stops,
          dropLocation = (\loc -> LatLong loc.lat loc.lon) <$> ride.toLocation,
          isManualArrival
        }
