{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TripLocation where

import qualified Domain.Types.TripLocation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TripLocationT sql=trip_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey TripLocationT where
  type DomainKey TripLocationT = Id Domain.TripLocation
  fromKey (TripLocationTKey _id) = Id _id
  toKey (Id id) = TripLocationTKey id

instance FromTType TripLocationT Domain.TripLocation where
  fromTType TripLocationT {..} = do
    return $
      Domain.TripLocation
        { id = Id id,
          address =
            Domain.LocationAddress {..},
          ..
        }

instance ToTType TripLocationT Domain.TripLocation where
  toTType Domain.TripLocation {..} =
    TripLocationT
      { id = getId id,
        street = address.street,
        city = address.city,
        state = address.state,
        country = address.country,
        building = address.building,
        areaCode = address.areaCode,
        area = address.area,
        ..
      }

mkDomainTripLocation :: TripLocationT -> Domain.TripLocation
mkDomainTripLocation TripLocationT {..} = do
  let address = Domain.LocationAddress {..}
  Domain.TripLocation
    { id = Id id,
      ..
    }

mkTabularTripLocation :: Domain.TripLocation -> TripLocationT
mkTabularTripLocation Domain.TripLocation {..} = do
  let Domain.LocationAddress {..} = address
  TripLocationT
    { id = getId id,
      ..
    }
