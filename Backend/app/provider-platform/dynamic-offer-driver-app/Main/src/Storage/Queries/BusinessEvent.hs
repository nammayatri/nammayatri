{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BusinessEvent where

import Domain.Types.Booking
import Domain.Types.BusinessEvent
import Domain.Types.Person (Driver)
import Domain.Types.Ride
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import Sequelize as Se
import qualified Storage.Beam.BusinessEvent as BeamBE
import Storage.Tabular.BusinessEvent ()

logBusinessEvent ::
  (L.MonadFlow m, MonadGuid m, MonadTime m) =>
  Maybe (Id Driver) ->
  EventType ->
  Maybe (Id Booking) ->
  Maybe WhenPoolWasComputed ->
  Maybe Variant ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe (Id Ride) ->
  m ()
logBusinessEvent driverId eventType bookingId whenPoolWasComputed variant distance duration rideId = do
  uuid <- generateGUID
  now <- getCurrentTime
  let bE =
        BusinessEvent
          { id = uuid,
            eventType = eventType,
            timeStamp = now,
            driverId = driverId,
            bookingId = bookingId,
            whenPoolWasComputed = whenPoolWasComputed,
            vehicleVariant = variant,
            distance = distance,
            duration = duration,
            rideId = rideId
          }
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamBE.BusinessEventT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainBusinessEventToBeam bE)
    Nothing -> pure ()

logDriverAssignedEvent :: (L.MonadFlow m, MonadGuid m, MonadTime m) => Id Driver -> Id Booking -> Id Ride -> m ()
logDriverAssignedEvent driverId bookingId rideId = do
  logBusinessEvent
    (Just driverId)
    DRIVER_ASSIGNED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

logRideConfirmedEvent :: (L.MonadFlow m, MonadGuid m, MonadTime m) => Id Booking -> m ()
logRideConfirmedEvent bookingId = do
  logBusinessEvent
    Nothing
    RIDE_CONFIRMED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

logRideCommencedEvent :: (L.MonadFlow m, MonadGuid m, MonadTime m) => Id Driver -> Id Booking -> Id Ride -> m ()
logRideCommencedEvent driverId bookingId rideId = do
  logBusinessEvent
    (Just driverId)
    RIDE_COMMENCED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

transformBeamBusinessEventToDomain :: BeamBE.BusinessEvent -> BusinessEvent
transformBeamBusinessEventToDomain BeamBE.BusinessEventT {..} = do
  BusinessEvent
    { id = Id id,
      driverId = Id <$> driverId,
      eventType = eventType,
      timeStamp = timeStamp,
      bookingId = Id <$> bookingId,
      whenPoolWasComputed = whenPoolWasComputed,
      vehicleVariant = vehicleVariant,
      distance = Meters <$> distance,
      duration = Seconds <$> distance,
      rideId = Id <$> rideId
    }

transformDomainBusinessEventToBeam :: BusinessEvent -> BeamBE.BusinessEvent
transformDomainBusinessEventToBeam BusinessEvent {..} =
  BeamBE.defaultBusinessEvent
    { BeamBE.id = getId id,
      BeamBE.driverId = getId <$> driverId,
      BeamBE.eventType = eventType,
      BeamBE.timeStamp = timeStamp,
      BeamBE.bookingId = getId <$> bookingId,
      BeamBE.whenPoolWasComputed = whenPoolWasComputed,
      BeamBE.vehicleVariant = vehicleVariant,
      BeamBE.distance = getMeters <$> distance,
      BeamBE.duration = getSeconds <$> duration,
      BeamBE.rideId = getId <$> rideId
    }
