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
          { id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            street = street,
            door = door,
            city = city,
            state = state,
            country = country,
            building = building,
            areaCode = areaCode,
            area = area,
            createdAt = createdAt,
            updatedAt = updatedAt,
            tag = tag,
            isMoved = isMoved,
            riderId = Kernel.Types.Id.Id riderId,
            placeId = placeId,
            ward = ward
          }

instance ToTType' Beam.SavedReqLocation Domain.Types.SavedReqLocation.SavedReqLocation where
  toTType' (Domain.Types.SavedReqLocation.SavedReqLocation {..}) = do
    Beam.SavedReqLocationT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.street = street,
        Beam.door = door,
        Beam.city = city,
        Beam.state = state,
        Beam.country = country,
        Beam.building = building,
        Beam.areaCode = areaCode,
        Beam.area = area,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.tag = tag,
        Beam.isMoved = isMoved,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.placeId = placeId,
        Beam.ward = ward
      }
