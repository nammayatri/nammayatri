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
import Domain.Types.BookingCancellationReason
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create

findByRideBookingId ::
  Transactionable m =>
  Id Booking ->
  m (Maybe BookingCancellationReason)
findByRideBookingId rideBookingId =
  Esq.findOne $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ rideBookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey rideBookingId)
    return rideBookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB ()
upsert cancellationReason =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonSource =. val (cancellationReason.source),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonReasonStage =. val (cancellationReason.reasonStage),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]

upsert' :: L.MonadFlow m => BookingCancellationReason -> m ()
upsert' cancellationReason = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamBCR.BookingCancellationReasonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- either (pure Nothing) (transformBeamBookingCancellationReasonToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              updatedMeshConfig
              [ Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
                Se.Set BeamBCR.source cancellationReason.source,
                Se.Set BeamBCR.reasonCode cancellationReason.reasonCode,
                Se.Set BeamBCR.reasonStage cancellationReason.reasonStage,
                Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
              ]
              [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
        else void $ KV.createWoReturingKVConnector dbCOnf' updatedMeshConfig (transformDomainBookingCancellationReasonToBeam cancellationReason)
    Nothing -> pure ()

transformBeamBookingCancellationReasonToDomain :: BeamBCR.BookingCancellationReason -> BookingCancellationReason
transformBeamBookingCancellationReasonToDomain BeamBCR.BookingCancellationReasonT {..} = do
  BookingCancellationReason
    { bookingId = Id bookingId,
      rideId = Id <$> rideId,
      merchantId = Id <$> merchantId,
      source = source,
      reasonCode = reasonCode,
      reasonStage = reasonStage,
      additionalInfo = additionalInfo,
      driverCancellationLocation = LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
      driverDistToPickup = driverDistToPickup
    }

transformDomainBookingCancellationReasonToBeam :: BookingCancellationReason -> BeamBCR.BookingCancellationReason
transformDomainBookingCancellationReasonToBeam BookingCancellationReason {..} =
  BeamBCR.BookingCancellationReasonT
    { BeamBCR.bookingId = getId bookingId,
      BeamBCR.rideId = getId <$> rideId,
      BeamBCR.merchantId = getId <$> merchantId,
      BeamBCR.source = source,
      BeamBCR.reasonStage = reasonStage,
      BeamBCR.reasonCode = reasonCode,
      BeamBCR.additionalInfo = additionalInfo,
      BeamBCR.driverCancellationLocationLat = driverCancellationLocation <&> (.lat),
      BeamBCR.driverCancellationLocationLon = driverCancellationLocation <&> (.lon),
      BeamBCR.driverDistToPickup = driverDistToPickup
    }
