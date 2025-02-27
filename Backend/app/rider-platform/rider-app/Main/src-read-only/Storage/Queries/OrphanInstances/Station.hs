{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Station where

import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Station as Beam

instance FromTType' Beam.Station Domain.Types.Station.Station where
  fromTType' (Beam.StationT {..}) = do
    pure $
      Just
        Domain.Types.Station.Station
          { address = address,
            code = code,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            possibleTypes = possibleTypes,
            timeBounds = fromMaybe Kernel.Types.TimeBound.Unbounded timeBounds,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Station Domain.Types.Station.Station where
  toTType' (Domain.Types.Station.Station {..}) = do
    Beam.StationT
      { Beam.address = address,
        Beam.code = code,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.possibleTypes = possibleTypes,
        Beam.timeBounds = Kernel.Prelude.Just timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
