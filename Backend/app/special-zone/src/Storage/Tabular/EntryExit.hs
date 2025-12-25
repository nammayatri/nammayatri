{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.EntryExit where

import qualified Domain.Types.SpecialZone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.SpecialZone (SpecialZoneTId)

derivePersistField "Domain.EntryExitType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    EntryExitT sql=entry_exit
      id Text
      specialZoneId SpecialZoneTId
      entryExitType Domain.EntryExitType
      lat Double
      lon Double
      area Text Maybe
      address Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey EntryExitT where
  type DomainKey EntryExitT = Id Domain.EntryExit
  fromKey (EntryExitTKey _id) = Id _id
  toKey (Id id) = EntryExitTKey id

instance FromTType EntryExitT Domain.EntryExit where
  fromTType EntryExitT {..} =
    return $
      Domain.EntryExit
        { id = Id id,
          specialZoneId = fromKey specialZoneId,
          _type = entryExitType,
          ..
        }

instance ToTType EntryExitT Domain.EntryExit where
  toTType Domain.EntryExit {..} =
    EntryExitT
      { id = getId id,
        specialZoneId = toKey specialZoneId,
        entryExitType = _type,
        ..
      }
