{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Location where

import Domain.Types.Location
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Location as BeamL

create :: MonadFlow m => Location -> m ()
create = createWithKV

findById :: MonadFlow m => Id Location -> m (Maybe Location)
findById (Id locationId) = findOneWithKV [Se.Is BeamL.id $ Se.Eq locationId]

updateAddress :: MonadFlow m => Id Location -> LocationAddress -> m ()
updateAddress (Id blId) LocationAddress {..} = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamL.street street,
      Se.Set BeamL.door door,
      Se.Set BeamL.city city,
      Se.Set BeamL.state state,
      Se.Set BeamL.country country,
      Se.Set BeamL.building building,
      Se.Set BeamL.areaCode areaCode,
      Se.Set BeamL.area area,
      Se.Set BeamL.fullAddress fullAddress,
      Se.Set BeamL.updatedAt now
    ]
    [Se.Is BeamL.id (Se.Eq blId)]

getBookingLocs ::
  (MonadFlow m) =>
  [Id Location] ->
  m [Location]
getBookingLocs locationIds = findAllWithKV [Se.Is BeamL.id $ Se.In $ getId <$> locationIds]

instance FromTType' BeamL.Location Location where
  fromTType' BeamL.LocationT {..} = do
    pure $
      Just
        Location
          { id = Id id,
            lat = lat,
            lon = lon,
            address = LocationAddress street door city state country building areaCode area fullAddress,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamL.Location Location where
  toTType' Location {..} = do
    BeamL.LocationT
      { BeamL.id = getId id,
        BeamL.lat = lat,
        BeamL.lon = lon,
        BeamL.street = (street :: LocationAddress -> Maybe Text) address,
        BeamL.door = (door :: LocationAddress -> Maybe Text) address,
        BeamL.city = (city :: LocationAddress -> Maybe Text) address,
        BeamL.state = (state :: LocationAddress -> Maybe Text) address,
        BeamL.country = (country :: LocationAddress -> Maybe Text) address,
        BeamL.building = (building :: LocationAddress -> Maybe Text) address,
        BeamL.areaCode = (areaCode :: LocationAddress -> Maybe Text) address,
        BeamL.area = (area :: LocationAddress -> Maybe Text) address,
        BeamL.fullAddress = (fullAddress :: LocationAddress -> Maybe Text) address,
        BeamL.createdAt = createdAt,
        BeamL.updatedAt = updatedAt
      }
