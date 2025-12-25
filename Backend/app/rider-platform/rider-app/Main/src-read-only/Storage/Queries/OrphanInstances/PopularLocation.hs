{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PopularLocation where

import qualified Domain.Types.PopularLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PopularLocation as Beam

instance FromTType' Beam.PopularLocation Domain.Types.PopularLocation.PopularLocation where
  fromTType' (Beam.PopularLocationT {..}) = do
    pure $
      Just
        Domain.Types.PopularLocation.PopularLocation
          { address = address,
            createdAt = createdAt,
            id = id,
            lat = lat,
            lon = lon,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            rating = rating,
            type_ = type_,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.PopularLocation Domain.Types.PopularLocation.PopularLocation where
  toTType' (Domain.Types.PopularLocation.PopularLocation {..}) = do
    Beam.PopularLocationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.id = id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.rating = rating,
        Beam.type_ = type_,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
