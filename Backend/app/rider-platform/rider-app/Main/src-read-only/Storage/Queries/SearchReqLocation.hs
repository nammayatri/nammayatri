{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchReqLocation where

import qualified Domain.Types.LocationAddress
import qualified Domain.Types.SearchReqLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchReqLocation as Beam
import Storage.Queries.Transformers.SearchReqLocation

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchReqLocation.SearchReqLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchReqLocation.SearchReqLocation] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation -> m (Maybe Domain.Types.SearchReqLocation.SearchReqLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchReqLocation.SearchReqLocation -> m ())
updateByPrimaryKey (Domain.Types.SearchReqLocation.SearchReqLocation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.area (Domain.Types.LocationAddress.area address),
      Se.Set Beam.areaCode (Domain.Types.LocationAddress.areaCode address),
      Se.Set Beam.building (Domain.Types.LocationAddress.building address),
      Se.Set Beam.city (Domain.Types.LocationAddress.city address),
      Se.Set Beam.country (Domain.Types.LocationAddress.country address),
      Se.Set Beam.door (Domain.Types.LocationAddress.door address),
      Se.Set Beam.extras (Domain.Types.LocationAddress.extras address),
      Se.Set Beam.instructions (Domain.Types.LocationAddress.instructions address),
      Se.Set Beam.placeId (Domain.Types.LocationAddress.placeId address),
      Se.Set Beam.state (Domain.Types.LocationAddress.state address),
      Se.Set Beam.street (Domain.Types.LocationAddress.street address),
      Se.Set Beam.title (Domain.Types.LocationAddress.title address),
      Se.Set Beam.ward (Domain.Types.LocationAddress.ward address),
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SearchReqLocation Domain.Types.SearchReqLocation.SearchReqLocation where
  fromTType' (Beam.SearchReqLocationT {..}) = do
    pure $
      Just
        Domain.Types.SearchReqLocation.SearchReqLocation
          { address = mkLocationAddress area areaCode building city country door extras instructions placeId state street title ward,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SearchReqLocation Domain.Types.SearchReqLocation.SearchReqLocation where
  toTType' (Domain.Types.SearchReqLocation.SearchReqLocation {..}) = do
    Beam.SearchReqLocationT
      { Beam.area = Domain.Types.LocationAddress.area address,
        Beam.areaCode = Domain.Types.LocationAddress.areaCode address,
        Beam.building = Domain.Types.LocationAddress.building address,
        Beam.city = Domain.Types.LocationAddress.city address,
        Beam.country = Domain.Types.LocationAddress.country address,
        Beam.door = Domain.Types.LocationAddress.door address,
        Beam.extras = Domain.Types.LocationAddress.extras address,
        Beam.instructions = Domain.Types.LocationAddress.instructions address,
        Beam.placeId = Domain.Types.LocationAddress.placeId address,
        Beam.state = Domain.Types.LocationAddress.state address,
        Beam.street = Domain.Types.LocationAddress.street address,
        Beam.title = Domain.Types.LocationAddress.title address,
        Beam.ward = Domain.Types.LocationAddress.ward address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.updatedAt = updatedAt
      }
