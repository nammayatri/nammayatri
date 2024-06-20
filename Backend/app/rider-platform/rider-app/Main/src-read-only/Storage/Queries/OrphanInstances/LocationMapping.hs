{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.LocationMapping where

import qualified Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.LocationMapping as Beam

instance FromTType' Beam.LocationMapping Domain.Types.LocationMapping.LocationMapping where
  fromTType' (Beam.LocationMappingT {..}) = do
    pure $
      Just
        Domain.Types.LocationMapping.LocationMapping
          { id = Kernel.Types.Id.Id id,
            tag = tag,
            locationId = Kernel.Types.Id.Id locationId,
            entityId = entityId,
            order = order,
            version = version,
            createdAt = createdAt,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.LocationMapping Domain.Types.LocationMapping.LocationMapping where
  toTType' (Domain.Types.LocationMapping.LocationMapping {..}) = do
    Beam.LocationMappingT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.tag = tag,
        Beam.locationId = Kernel.Types.Id.getId locationId,
        Beam.entityId = entityId,
        Beam.order = order,
        Beam.version = version,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
