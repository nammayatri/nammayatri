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

module Lib.Tabular.SpecialLocationPriority where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocationPriority as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialLocationPriorityT sql=special_location_priority
      id Text
      merchantId Text
      merchantOperatingCityId Text
      category Text
      pickupPriority Int
      dropPriority Int
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialLocationPriorityT where
  type DomainKey SpecialLocationPriorityT = Id Domain.SpecialLocationPriority
  fromKey (SpecialLocationPriorityTKey _id) = Id _id
  toKey (Id id) = SpecialLocationPriorityTKey id

instance FromTType SpecialLocationPriorityT Domain.SpecialLocationPriority where
  fromTType SpecialLocationPriorityT {..} =
    return $
      Domain.SpecialLocationPriority
        { id = Id id,
          ..
        }

instance ToTType SpecialLocationPriorityT Domain.SpecialLocationPriority where
  toTType Domain.SpecialLocationPriority {..} =
    SpecialLocationPriorityT
      { id = getId id,
        ..
      }
