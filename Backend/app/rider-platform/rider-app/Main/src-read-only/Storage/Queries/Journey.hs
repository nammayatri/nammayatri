{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Journey where

import qualified Domain.Types.Journey
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Journey.Journey] -> m ())
createMany = traverse_ create

findBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m [Domain.Types.Journey.Journey])
findBySearchId searchRequestId = do findAllWithKV [Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)]

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
      Se.Set Beam.modes modes,
      Se.Set Beam.searchRequestId (Kernel.Types.Id.getId searchRequestId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.totalLegs totalLegs,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Journey Domain.Types.Journey.Journey where
  fromTType' (Beam.JourneyT {..}) = do
    pure $
      Just
        Domain.Types.Journey.Journey
          { convenienceCost = convenienceCost,
            endTime = endTime,
            estimatedDistance = Kernel.Types.Common.Distance estimatedDistance distanceUnit,
            estimatedDuration = estimatedDuration,
            id = Kernel.Types.Id.Id id,
            modes = modes,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            startTime = startTime,
            totalLegs = totalLegs,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Journey Domain.Types.Journey.Journey where
  toTType' (Domain.Types.Journey.Journey {..}) = do
    Beam.JourneyT
      { Beam.convenienceCost = convenienceCost,
        Beam.endTime = endTime,
        Beam.distanceUnit = (.unit) estimatedDistance,
        Beam.estimatedDistance = (.value) estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.modes = modes,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.startTime = startTime,
        Beam.totalLegs = totalLegs,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
