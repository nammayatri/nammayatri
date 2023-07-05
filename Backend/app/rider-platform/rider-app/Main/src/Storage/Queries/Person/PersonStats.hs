{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.PersonStats where

import Domain.Types.Person
import qualified Domain.Types.Person.PersonStats as DPS
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Person.PersonStats

create :: DPS.PersonStats -> SqlDB ()
create = Esq.create

findByPersonId ::
  (Transactionable m) =>
  Id Person ->
  m (Maybe DPS.PersonStats)
findByPersonId personId = do
  Esq.findOne $ do
    personStats <- from $ table @PersonStatsT
    where_ $
      personStats ^. PersonStatsTId ==. val (toKey personId)
    return personStats

incrementUserCancelledRidesCount :: Id Person -> SqlDB ()
incrementUserCancelledRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsUserCancelledRides =. (tbl ^. PersonStatsUserCancelledRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementDriverCancelledRidesCount :: Id Person -> SqlDB ()
incrementDriverCancelledRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsDriverCancelledRides =. (tbl ^. PersonStatsDriverCancelledRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementCompletedRidesCount :: Id Person -> SqlDB ()
incrementCompletedRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsCompletedRides =. (tbl ^. PersonStatsCompletedRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementWeekendRidesCount :: Id Person -> SqlDB ()
incrementWeekendRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsWeekendRides =. (tbl ^. PersonStatsWeekendRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementWeekdayRidesCount :: Id Person -> SqlDB ()
incrementWeekdayRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsWeekdayRides =. (tbl ^. PersonStatsWeekdayRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementOffpeakRidesCount :: Id Person -> SqlDB ()
incrementOffpeakRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsOffPeakRides =. (tbl ^. PersonStatsOffPeakRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementEveningPeakRidesCount :: Id Person -> SqlDB ()
incrementEveningPeakRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsEveningPeakRides =. (tbl ^. PersonStatsEveningPeakRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)

incrementMorningPeakRidesCount :: Id Person -> SqlDB ()
incrementMorningPeakRidesCount personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonStatsUpdatedAt =. val now,
        PersonStatsMorningPeakRides =. (tbl ^. PersonStatsMorningPeakRides +. val 1)
      ]
    where_ $ tbl ^. PersonStatsTId ==. val (toKey personId)
