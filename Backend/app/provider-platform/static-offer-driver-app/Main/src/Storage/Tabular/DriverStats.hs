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

module Storage.Tabular.DriverStats where

import qualified Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverStatsT sql=driver_stats
      driverId PersonTId
      idleSince UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverStatsT where
  type DomainKey DriverStatsT = Id Driver
  fromKey (DriverStatsTKey _id) = cast $ fromKey _id
  toKey id = DriverStatsTKey . toKey $ cast id

instance FromTType DriverStatsT Domain.DriverStats where
  fromTType DriverStatsT {..} = do
    return $
      Domain.DriverStats
        { driverId = cast $ fromKey driverId,
          ..
        }

instance ToTType DriverStatsT Domain.DriverStats where
  toTType Domain.DriverStats {..} =
    DriverStatsT
      { driverId = toKey . cast $ driverId,
        ..
      }
