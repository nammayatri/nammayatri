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
          { createdAt = createdAt,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            locationId = Kernel.Types.Id.Id locationId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            order = order,
            tag = tag,
            updatedAt = updatedAt,
            version = version
          }

instance ToTType' Beam.LocationMapping Domain.Types.LocationMapping.LocationMapping where
  toTType' (Domain.Types.LocationMapping.LocationMapping {..}) = do
    Beam.LocationMappingT
      { Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.locationId = Kernel.Types.Id.getId locationId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.order = order,
        Beam.tag = tag,
        Beam.updatedAt = updatedAt,
        Beam.version = version
      }
