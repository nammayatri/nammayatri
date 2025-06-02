{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BusinessHour where

import qualified Domain.Types.BusinessHour
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BusinessHour as Beam

instance FromTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  fromTType' (Beam.BusinessHourT {..}) = do
    pure $
      Just
        Domain.Types.BusinessHour.BusinessHour
          { bookingClosingTime = bookingClosingTime,
            btype = btype,
            categoryId = Kernel.Types.Id.Id <$> categoryId,
            expiryDate = expiryDate,
            hash = hash,
            id = Kernel.Types.Id.Id id,
            name = name,
            placeId = placeId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  toTType' (Domain.Types.BusinessHour.BusinessHour {..}) = do
    Beam.BusinessHourT
      { Beam.bookingClosingTime = bookingClosingTime,
        Beam.btype = btype,
        Beam.categoryId = Kernel.Types.Id.getId <$> categoryId,
        Beam.expiryDate = expiryDate,
        Beam.hash = hash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.placeId = placeId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
