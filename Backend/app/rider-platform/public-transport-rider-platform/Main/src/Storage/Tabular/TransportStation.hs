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

module Storage.Tabular.TransportStation where

import qualified Domain.Types.TransportStation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransportStationT sql=transport_station
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      Primary id
      deriving Generic
    |]

instance TEntityKey TransportStationT where
  type DomainKey TransportStationT = Id Domain.TransportStation
  fromKey (TransportStationTKey _id) = Id _id
  toKey id = TransportStationTKey id.getId

instance FromTType TransportStationT Domain.TransportStation where
  fromTType TransportStationT {..} = do
    return $
      Domain.TransportStation
        { id = Id id,
          ..
        }

instance ToTType TransportStationT Domain.TransportStation where
  toTType Domain.TransportStation {..} =
    TransportStationT
      { id = id.getId,
        ..
      }
