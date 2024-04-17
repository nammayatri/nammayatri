{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Location where

import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Location as Beam

create :: KvDbFlow m r => (Domain.Types.Location.Location -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Location.Location] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Location.Location -> m (Maybe Domain.Types.Location.Location))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateAddress :: KvDbFlow m r => (Domain.Types.LocationAddress.LocationAddress -> Kernel.Types.Id.Id Domain.Types.Location.Location -> m ())
updateAddress address (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.area (Domain.Types.LocationAddress.area address),
      Se.Set Beam.areaCode (Domain.Types.LocationAddress.areaCode address),
      Se.Set Beam.building (Domain.Types.LocationAddress.building address),
      Se.Set Beam.city (Domain.Types.LocationAddress.city address),
      Se.Set Beam.country (Domain.Types.LocationAddress.country address),
      Se.Set Beam.door (Domain.Types.LocationAddress.door address),
      Se.Set Beam.placeId (Domain.Types.LocationAddress.placeId address),
      Se.Set Beam.state (Domain.Types.LocationAddress.state address),
      Se.Set Beam.street (Domain.Types.LocationAddress.street address),
      Se.Set Beam.ward (Domain.Types.LocationAddress.ward address),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Location.Location -> m (Maybe Domain.Types.Location.Location))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Location.Location -> m ())
updateByPrimaryKey (Domain.Types.Location.Location {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.area (Domain.Types.LocationAddress.area address),
      Se.Set Beam.areaCode (Domain.Types.LocationAddress.areaCode address),
      Se.Set Beam.building (Domain.Types.LocationAddress.building address),
      Se.Set Beam.city (Domain.Types.LocationAddress.city address),
      Se.Set Beam.country (Domain.Types.LocationAddress.country address),
      Se.Set Beam.door (Domain.Types.LocationAddress.door address),
      Se.Set Beam.placeId (Domain.Types.LocationAddress.placeId address),
      Se.Set Beam.state (Domain.Types.LocationAddress.state address),
      Se.Set Beam.street (Domain.Types.LocationAddress.street address),
      Se.Set Beam.ward (Domain.Types.LocationAddress.ward address),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Location Domain.Types.Location.Location where
  fromTType' (Beam.LocationT {..}) = do
    pure $
      Just
        Domain.Types.Location.Location
          { address = Domain.Types.LocationAddress.LocationAddress {street, door, city, state, country, building, areaCode, area, ward, placeId},
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Location Domain.Types.Location.Location where
  toTType' (Domain.Types.Location.Location {..}) = do
    Beam.LocationT
      { Beam.area = Domain.Types.LocationAddress.area address,
        Beam.areaCode = Domain.Types.LocationAddress.areaCode address,
        Beam.building = Domain.Types.LocationAddress.building address,
        Beam.city = Domain.Types.LocationAddress.city address,
        Beam.country = Domain.Types.LocationAddress.country address,
        Beam.door = Domain.Types.LocationAddress.door address,
        Beam.placeId = Domain.Types.LocationAddress.placeId address,
        Beam.state = Domain.Types.LocationAddress.state address,
        Beam.street = Domain.Types.LocationAddress.street address,
        Beam.ward = Domain.Types.LocationAddress.ward address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.updatedAt = updatedAt
      }
