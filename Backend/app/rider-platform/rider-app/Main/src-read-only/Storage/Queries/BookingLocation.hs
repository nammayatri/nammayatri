{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingLocation where

import qualified Domain.Types.BookingLocation
import qualified Domain.Types.LocationAddress
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingLocation as Beam

create :: KvDbFlow m r => (Domain.Types.BookingLocation.BookingLocation -> m ())
create = createWithKV

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BookingLocation.BookingLocation -> m (Maybe Domain.Types.BookingLocation.BookingLocation))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateAddress :: KvDbFlow m r => (Domain.Types.LocationAddress.LocationAddress -> Kernel.Types.Id.Id Domain.Types.BookingLocation.BookingLocation -> m ())
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

instance FromTType' Beam.BookingLocation Domain.Types.BookingLocation.BookingLocation where
  fromTType' (Beam.BookingLocationT {..}) = do
    pure $
      Just
        Domain.Types.BookingLocation.BookingLocation
          { address = Domain.Types.LocationAddress.LocationAddress {street, door, city, state, country, building, areaCode, area, ward, placeId},
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BookingLocation Domain.Types.BookingLocation.BookingLocation where
  toTType' (Domain.Types.BookingLocation.BookingLocation {..}) = do
    Beam.BookingLocationT
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
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
