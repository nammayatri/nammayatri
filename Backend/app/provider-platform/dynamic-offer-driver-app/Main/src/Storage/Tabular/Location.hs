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

module Storage.Tabular.Location where

import qualified Domain.Types.Location as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    LocationT sql=location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      fullAddress Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey LocationT where
  type DomainKey LocationT = Id Domain.Location
  fromKey (LocationTKey _id) = Id _id
  toKey (Id id) = LocationTKey id

instance FromTType LocationT Domain.Location where
  fromTType LocationT {..} = do
    let address = Domain.LocationAddress {..}
    return $
      Domain.Location
        { id = Id id,
          ..
        }

instance ToTType LocationT Domain.Location where
  toTType Domain.Location {..} = do
    let Domain.LocationAddress {..} = address
    LocationT
      { id = getId id,
        ..
      }

mkDomainBookingLocation :: LocationT -> Domain.Location
mkDomainBookingLocation LocationT {..} = do
  let address = Domain.LocationAddress {..}
  Domain.Location
    { id = Id id,
      ..
    }

mkTabularBookingLocation :: Domain.Location -> LocationT
mkTabularBookingLocation Domain.Location {..} = do
  let Domain.LocationAddress {..} = address
  LocationT
    { id = getId id,
      ..
    }
