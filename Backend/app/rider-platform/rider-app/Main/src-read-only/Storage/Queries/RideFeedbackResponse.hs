{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideFeedbackResponse (module Storage.Queries.RideFeedbackResponse, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.Ride
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RideFeedbackResponse as Beam
import Storage.Queries.RideFeedbackResponseExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse] -> m ())
createMany = traverse_ create

findAllByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse]))
findAllByRideId rideId = do findAllWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

findByRideIdAndConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> m (Maybe Domain.Types.RideFeedbackResponse.RideFeedbackResponse))
findByRideIdAndConfigId rideId configId = do findOneWithKV [Se.And [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId), Se.Is Beam.configId $ Se.Eq (Kernel.Types.Id.getId configId)]]

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
      Se.Set Beam.configVersion configVersion,
      Se.Set Beam.lat lat,
      Se.Set Beam.logicVersion logicVersion,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.parentResponseId (Kernel.Types.Id.getId <$> parentResponseId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.questionKey questionKey,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.rideStatusAtResponse (Kernel.Prelude.show <$> rideStatusAtResponse),
      Se.Set Beam.secondsIntoRide secondsIntoRide,
      Se.Set Beam.selectedOptionKeys selectedOptionKeys,
      Se.Set Beam.shownCount shownCount,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
