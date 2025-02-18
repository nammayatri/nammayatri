{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SavedReqLocation where

import qualified Domain.Types.SavedReqLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SavedReqLocation as Beam

instance FromTType' Beam.SavedReqLocation Domain.Types.SavedReqLocation.SavedReqLocation where
  fromTType' (Beam.SavedReqLocationT {..}) = do
    pure $
      Just
        Domain.Types.SavedReqLocation.SavedReqLocation
          { area = area,
            areaCode = areaCode,
            building = building,
            city = city,
            country = country,
            createdAt = createdAt,
            door = door,
            id = Kernel.Types.Id.Id id,
            isMoved = isMoved,
            lat = lat,
            locationName = locationName,
            lon = lon,
            placeId = placeId,
            riderId = Kernel.Types.Id.Id riderId,
            state = state,
            street = street,
            tag = tag,
            updatedAt = updatedAt,
            ward = ward
          }

instance ToTType' Beam.SavedReqLocation Domain.Types.SavedReqLocation.SavedReqLocation where
  toTType' (Domain.Types.SavedReqLocation.SavedReqLocation {..}) = do
    Beam.SavedReqLocationT
      { Beam.area = area,
        Beam.areaCode = areaCode,
        Beam.building = building,
        Beam.city = city,
        Beam.country = country,
        Beam.createdAt = createdAt,
        Beam.door = door,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isMoved = isMoved,
        Beam.lat = lat,
        Beam.locationName = locationName,
        Beam.lon = lon,
        Beam.placeId = placeId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.state = state,
        Beam.street = street,
        Beam.tag = tag,
        Beam.updatedAt = updatedAt,
        Beam.ward = ward
      }
