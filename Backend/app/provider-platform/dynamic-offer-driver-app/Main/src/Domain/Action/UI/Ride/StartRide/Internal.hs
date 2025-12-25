{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.StartRide.Internal (startRideTransaction) where

import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant as Dmerch
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, HasShortDurationRetryCfg, logTagInfo)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Ride as QRide
import Tools.Event

startRideTransaction ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r,
    LT.HasLocationService m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "minDistanceBetweenTwoPoints" r Int,
    HasShortDurationRetryCfg r c
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
  triggerRideStartEvent RideEventData {ride = ride{status = SRide.INPROGRESS}, personId = driverId, merchantId = merchantId}
  minDistanceBetweenTwoPoints <- bool (pure Nothing) (Just <$> asks (.minDistanceBetweenTwoPoints)) (ride.tripCategory == DTC.OneWay DTC.MeterRide)
  void $ LF.rideStart ride.id firstPoint.lat firstPoint.lon merchantId driverId (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = firstPoint, minDistanceBetweenTwoPoints = minDistanceBetweenTwoPoints, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) ride.stops}))
  QRide.updateStartTimeAndLoc ride.id firstPoint
  whenJust odometer $ \odometerReading -> QRide.updateStartOdometerReading ride.id odometerReading
  logTagInfo "startRide" ("Updating driver_flow_status to ON_RIDE for DriverId " <> getId driverId)
  DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig driverInfo.active Nothing DDFS.ON_RIDE driverInfo (Just True)
  QRide.updateStatus ride.id SRide.INPROGRESS
  QBE.logRideCommencedEvent (cast driverId) bookingId ride.id ride.distanceUnit
