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

module Storage.Tabular.SavedReqLocation where

import qualified Domain.Types.SavedReqLocation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Person as Person

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SavedReqLocationT sql=saved_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      placeId Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      tag  Text
      riderId Person.PersonTId
      ward Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey SavedReqLocationT where
  type DomainKey SavedReqLocationT = Id Domain.SavedReqLocation
  fromKey (SavedReqLocationTKey _id) = Id _id
  toKey (Id id) = SavedReqLocationTKey id

instance FromTType SavedReqLocationT Domain.SavedReqLocation where
  fromTType SavedReqLocationT {..} = do
    return $
      Domain.SavedReqLocation
        { id = Id id,
          riderId = fromKey riderId,
          ..
        }

instance ToTType SavedReqLocationT Domain.SavedReqLocation where
  toTType Domain.SavedReqLocation {..} =
    SavedReqLocationT
      { id = getId id,
        riderId = toKey riderId,
        ..
      }
