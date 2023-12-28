{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Booking.BookingLocation where

import Domain.Types.Booking.BookingLocation
import Domain.Types.LocationAddress
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Booking.BookingLocation as BeamBL

create :: MonadFlow m => BookingLocation -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BookingLocation -> m (Maybe BookingLocation)
findById (Id bookingLocationId) = findOneWithKV [Se.Is BeamBL.id $ Se.Eq bookingLocationId]

updateAddress :: MonadFlow m => Id BookingLocation -> LocationAddress -> m ()
updateAddress (Id blId) LocationAddress {..} = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamBL.street street,
      Se.Set BeamBL.city city,
      Se.Set BeamBL.state state,
      Se.Set BeamBL.country country,
      Se.Set BeamBL.building building,
      Se.Set BeamBL.areaCode areaCode,
      Se.Set BeamBL.area area,
      Se.Set BeamBL.ward ward,
      Se.Set BeamBL.placeId placeId,
      Se.Set BeamBL.updatedAt now
    ]
    [Se.Is BeamBL.id (Se.Eq blId)]

instance FromTType' BeamBL.BookingLocation BookingLocation where
  fromTType' BeamBL.BookingLocationT {..} = do
    pure $
      Just
        BookingLocation
          { id = Id id,
            lat = lat,
            lon = lon,
            address = LocationAddress street door city state country building areaCode area ward placeId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamBL.BookingLocation BookingLocation where
  toTType' BookingLocation {..} = do
    BeamBL.BookingLocationT
      { BeamBL.id = getId id,
        BeamBL.lat = lat,
        BeamBL.lon = lon,
        BeamBL.street = (street :: LocationAddress -> Maybe Text) address,
        BeamBL.door = (door :: LocationAddress -> Maybe Text) address,
        BeamBL.city = (city :: LocationAddress -> Maybe Text) address,
        BeamBL.state = (state :: LocationAddress -> Maybe Text) address,
        BeamBL.country = (country :: LocationAddress -> Maybe Text) address,
        BeamBL.building = (building :: LocationAddress -> Maybe Text) address,
        BeamBL.areaCode = (areaCode :: LocationAddress -> Maybe Text) address,
        BeamBL.area = (area :: LocationAddress -> Maybe Text) address,
        BeamBL.ward = (ward :: LocationAddress -> Maybe Text) address,
        BeamBL.placeId = (placeId :: LocationAddress -> Maybe Text) address,
        BeamBL.createdAt = createdAt,
        BeamBL.updatedAt = updatedAt
      }
