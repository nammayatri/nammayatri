{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Location where

import qualified Domain.Types.Location
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Location as Beam
import Storage.Queries.Transformers.Location

instance FromTType' Beam.Location Domain.Types.Location.Location where
  fromTType' (Beam.LocationT {..}) = do
    pure $
      Just
        Domain.Types.Location.Location
          { address = mkAddress area areaCode building city country door fullAddress state street,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Location Domain.Types.Location.Location where
  toTType' (Domain.Types.Location.Location {..}) = do
    Beam.LocationT
      { Beam.area = (.area) address,
        Beam.areaCode = (.areaCode) address,
        Beam.building = (.building) address,
        Beam.city = (.city) address,
        Beam.country = (.country) address,
        Beam.door = (.door) address,
        Beam.fullAddress = (.fullAddress) address,
        Beam.state = (.state) address,
        Beam.street = (.street) address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.updatedAt = updatedAt
      }

{-
	DSL Source Link: file://./../../../../spec/Storage/Location.yaml
-}
