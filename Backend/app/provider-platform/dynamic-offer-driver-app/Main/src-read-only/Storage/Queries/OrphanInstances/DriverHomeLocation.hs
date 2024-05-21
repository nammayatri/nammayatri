{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverHomeLocation where

import qualified Domain.Types.DriverHomeLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverHomeLocation as Beam

instance FromTType' Beam.DriverHomeLocation Domain.Types.DriverHomeLocation.DriverHomeLocation where
  fromTType' (Beam.DriverHomeLocationT {..}) = do
    pure $
      Just
        Domain.Types.DriverHomeLocation.DriverHomeLocation
          { address = address,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            tag = tag,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverHomeLocation Domain.Types.DriverHomeLocation.DriverHomeLocation where
  toTType' (Domain.Types.DriverHomeLocation.DriverHomeLocation {..}) = do
    Beam.DriverHomeLocationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.tag = tag,
        Beam.updatedAt = updatedAt
      }
