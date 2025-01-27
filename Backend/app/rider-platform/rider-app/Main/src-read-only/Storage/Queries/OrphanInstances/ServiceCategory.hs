{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ServiceCategory where

import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.ServiceCategory as Beam

instance FromTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  fromTType' (Beam.ServiceCategoryT {..}) = do
    pure $
      Just
        Domain.Types.ServiceCategory.ServiceCategory
          { allowedSeats = allowedSeats,
            availableSeats = availableSeats,
            description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            peopleCategory = Kernel.Types.Id.Id <$> peopleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  toTType' (Domain.Types.ServiceCategory.ServiceCategory {..}) = do
    Beam.ServiceCategoryT
      { Beam.allowedSeats = allowedSeats,
        Beam.availableSeats = availableSeats,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.peopleCategory = Kernel.Types.Id.getId <$> peopleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
