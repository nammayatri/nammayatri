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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Tabular.SpecialLocation where

import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocation as Domain

deriving instance Read LatLong
deriving instance Read Domain.GatesInfo

derivePersistField "LatLong"
derivePersistField "Domain.GatesInfo"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialLocationT sql=special_location
      id Text
      locationName Text
      category Text
      gates (PostgresList Domain.GatesInfo)
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialLocationT where
  type DomainKey SpecialLocationT = Id Domain.SpecialLocation
  fromKey (SpecialLocationTKey _id) = Id _id
  toKey (Id id) = SpecialLocationTKey id

instance FromTType SpecialLocationT Domain.SpecialLocation where
  fromTType SpecialLocationT {..} =
    return $
      Domain.SpecialLocation
        { id = Id id,
          gates = unPostgresList gates,
          ..
        }

instance ToTType SpecialLocationT Domain.SpecialLocation where
  toTType Domain.SpecialLocation {..} =
    SpecialLocationT
      { id = getId id,
        gates = PostgresList gates,
        ..
      }
