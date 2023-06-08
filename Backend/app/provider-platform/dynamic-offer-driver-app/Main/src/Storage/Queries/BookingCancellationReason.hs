{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
import Domain.Types.Ride
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import EulerHS.Prelude as P hiding ((^.))
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR

-- import Storage.Tabular.BookingCancellationReason

create :: L.MonadFlow m => DBCR.BookingCancellationReason -> m (MeshResult ())
create bookingCancellationReason = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainBookingCancellationReasonToBeam bookingCancellationReason)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- TODO: Convert this function
-- findAllCancelledByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m Int
-- findAllCancelledByDriverId driverId = do
--   mkCount <$> do
--     Esq.findAll $ do
--       rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
--       where_ $
--         rideBookingCancellationReason ^. BookingCancellationReasonDriverId ==. val (Just $ toKey driverId)
--           &&. rideBookingCancellationReason ^. BookingCancellationReasonSource ==. val ByDriver
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

findAllCancelledByDriverId :: L.MonadFlow m => Id Person -> m Int
findAllCancelledByDriverId driverId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      res <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId)],
            Se.Is BeamBCR.source $ Se.Eq ByDriver
          ]
      pure $ either (const 0) length res
    Nothing -> pure 0

findByRideBookingId :: L.MonadFlow m => Id Booking -> m (Maybe BookingCancellationReason)
findByRideBookingId (Id rideBookingId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamBookingCancellationReasonToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamBCR.bookingId $ Se.Eq rideBookingId]
    Nothing -> pure Nothing

findByRideId :: L.MonadFlow m => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId (Id rideId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamBookingCancellationReasonToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamBCR.rideId $ Se.Eq (Just rideId)]
    Nothing -> pure Nothing

upsert :: L.MonadFlow m => BookingCancellationReason -> m ()
upsert cancellationReason = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- either (pure Nothing) (transformBeamBookingCancellationReasonToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              Mesh.meshConfig
              [ Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
                Se.Set BeamBCR.reasonCode ((\(CancellationReasonCode x) -> x) <$> cancellationReason.reasonCode),
                Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
              ]
              [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
        else void $ KV.createWoReturingKVConnector dbCOnf' Mesh.meshConfig (transformDomainBookingCancellationReasonToBeam cancellationReason)
    Nothing -> pure ()

transformBeamBookingCancellationReasonToDomain :: BeamBCR.BookingCancellationReason -> BookingCancellationReason
transformBeamBookingCancellationReasonToDomain BeamBCR.BookingCancellationReasonT {..} = do
  BookingCancellationReason
    { driverId = Id <$> driverId,
      bookingId = Id bookingId,
      rideId = Id <$> rideId,
      source = source,
      reasonCode = CancellationReasonCode <$> reasonCode,
      additionalInfo = additionalInfo,
      driverCancellationLocation = LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
      driverDistToPickup = driverDistToPickup
    }

transformDomainBookingCancellationReasonToBeam :: BookingCancellationReason -> BeamBCR.BookingCancellationReason
transformDomainBookingCancellationReasonToBeam BookingCancellationReason {..} =
  BeamBCR.defaultBookingCancellationReason
    { BeamBCR.driverId = getId <$> driverId,
      BeamBCR.bookingId = getId bookingId,
      BeamBCR.rideId = getId <$> rideId,
      BeamBCR.source = source,
      BeamBCR.reasonCode = (\(CancellationReasonCode x) -> x) <$> reasonCode,
      BeamBCR.additionalInfo = additionalInfo,
      BeamBCR.driverCancellationLocationLat = lat <$> driverCancellationLocation,
      BeamBCR.driverCancellationLocationLon = lon <$> driverCancellationLocation,
      BeamBCR.driverDistToPickup = driverDistToPickup
    }
