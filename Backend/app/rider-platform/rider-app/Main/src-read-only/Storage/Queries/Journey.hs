{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Journey (module Storage.Queries.Journey, module ReExport) where

import qualified Domain.Types.Journey
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam
import Storage.Queries.JourneyExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Journey.Journey] -> m ())
createMany = traverse_ create

findBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m [Domain.Types.Journey.Journey])
findBySearchId searchRequestId = do findAllWithKV [Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)]

updatePaymentStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updatePaymentStatus isPaymentSuccess id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isPaymentSuccess isPaymentSuccess, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.JourneyStatus -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status (Just status), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m (Maybe Domain.Types.Journey.Journey))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
updateByPrimaryKey (Domain.Types.Journey.Journey {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.convenienceCost convenienceCost,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.distanceUnit ((.unit) estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.isPaymentSuccess isPaymentSuccess,
      Se.Set Beam.modes modes,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.searchRequestId (Kernel.Types.Id.getId searchRequestId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status (Just status),
      Se.Set Beam.totalLegs totalLegs,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
