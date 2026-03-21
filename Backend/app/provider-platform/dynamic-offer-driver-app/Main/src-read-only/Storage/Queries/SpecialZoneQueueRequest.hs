{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialZoneQueueRequest (module Storage.Queries.SpecialZoneQueueRequest, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQueueRequest as Beam
import Storage.Queries.SpecialZoneQueueRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest] -> m ())
createMany = traverse_ create

findActiveByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus -> m ([Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest]))
findActiveByDriverId driverId status = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.Eq status]]

updateResponse ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestResponse -> Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus -> Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest -> m ())
updateResponse response status id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.response response, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest -> m (Maybe Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest -> m ())
updateByPrimaryKey (Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.gateId gateId,
      Se.Set Beam.gateName gateName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.response response,
      Se.Set Beam.specialLocationId specialLocationId,
      Se.Set Beam.specialLocationName specialLocationName,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleType vehicleType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
