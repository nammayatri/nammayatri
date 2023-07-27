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

module Storage.Tabular.Person.PersonStats where

import Domain.Types.Person (Person)
import qualified Domain.Types.Person.PersonStats as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonStatsT sql=person_stats
      personId PersonTId
      userCancelledRides Int
      driverCancelledRides Int
      completedRides Int
      weekendRides Int
      weekdayRides Int
      offPeakRides Int
      eveningPeakRides Int
      morningPeakRides Int
      weekendPeakRides Int
      updatedAt UTCTime
      Primary personId

      deriving Generic
    |]

instance TEntityKey PersonStatsT where
  type DomainKey PersonStatsT = Id Person
  fromKey (PersonStatsTKey _id) = fromKey _id
  toKey id = PersonStatsTKey $ toKey id

instance FromTType PersonStatsT Domain.PersonStats where
  fromTType PersonStatsT {..} =
    return $
      Domain.PersonStats
        { personId = fromKey personId,
          ..
        }

instance ToTType PersonStatsT Domain.PersonStats where
  toTType Domain.PersonStats {..} =
    PersonStatsT
      { personId = toKey personId,
        ..
      }
