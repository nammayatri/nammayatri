{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideFeedbackResponse where

import qualified Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Ride
import qualified Domain.Types.RideFeedbackResponse
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.RideFeedbackResponse as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse] -> m ())
createMany = traverse_ create

findAllByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse]))
findAllByRideId rideId = do findAllWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

updateActionResults ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult] -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
updateActionResults actionResults id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m (Maybe Domain.Types.RideFeedbackResponse.RideFeedbackResponse))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
updateByPrimaryKey (Domain.Types.RideFeedbackResponse.RideFeedbackResponse {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults),
      Se.Set Beam.answer (Data.Aeson.toJSON <$> answer),
      Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.configId (Kernel.Types.Id.getId configId),
      Se.Set Beam.configPilotVersions configPilotVersions,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.lat lat,
      Se.Set Beam.logicVersion logicVersion,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.parentResponseId (Kernel.Types.Id.getId <$> parentResponseId),
      Se.Set Beam.questionKey questionKey,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.rideStatusAtResponse (Kernel.Prelude.show <$> rideStatusAtResponse),
      Se.Set Beam.secondsIntoRide secondsIntoRide,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  fromTType' (Beam.RideFeedbackResponseT {..}) = do
    pure $
      Just
        Domain.Types.RideFeedbackResponse.RideFeedbackResponse
          { actionResults = Kernel.Utils.JSON.valueToMaybe =<< actionResults,
            answer = Kernel.Utils.JSON.valueToMaybe =<< answer,
            bookingId = Kernel.Types.Id.Id bookingId,
            configId = Kernel.Types.Id.Id configId,
            configPilotVersions = configPilotVersions,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            logicVersion = logicVersion,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            parentResponseId = Kernel.Types.Id.Id <$> parentResponseId,
            questionKey = questionKey,
            rideId = Kernel.Types.Id.Id rideId,
            rideStatusAtResponse = (Kernel.Prelude.readMaybe . Data.Text.unpack =<< rideStatusAtResponse),
            secondsIntoRide = secondsIntoRide,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  toTType' (Domain.Types.RideFeedbackResponse.RideFeedbackResponse {..}) = do
    Beam.RideFeedbackResponseT
      { Beam.actionResults = Data.Aeson.toJSON <$> actionResults,
        Beam.answer = Data.Aeson.toJSON <$> answer,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.configId = Kernel.Types.Id.getId configId,
        Beam.configPilotVersions = configPilotVersions,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.logicVersion = logicVersion,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parentResponseId = Kernel.Types.Id.getId <$> parentResponseId,
        Beam.questionKey = questionKey,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.rideStatusAtResponse = Kernel.Prelude.show <$> rideStatusAtResponse,
        Beam.secondsIntoRide = secondsIntoRide,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
