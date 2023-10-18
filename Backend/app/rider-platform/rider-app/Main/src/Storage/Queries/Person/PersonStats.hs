{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person.PersonStats where

import Domain.Types.Person
import qualified Domain.Types.Person.PersonStats as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Person.PersonStats as BeamPS hiding (Id)

create :: MonadFlow m => Domain.PersonStats -> m ()
create = createWithKV

findByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Domain.PersonStats)
findByPersonId (Id personId) = findOneWithKV [Se.Is BeamPS.personId $ Se.Eq personId]

incrementOrSetPersonStats :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.PersonStats -> m ()
incrementOrSetPersonStats personStats = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamPS.personId (Se.Eq (getId personStats.personId))]
  case res of
    Nothing -> pure ()
    Just ps ->
      updateOneWithKV
        [ Se.Set BeamPS.updatedAt now,
          Se.Set BeamPS.userCancelledRides (ps.userCancelledRides + personStats.userCancelledRides),
          Se.Set BeamPS.driverCancelledRides (ps.driverCancelledRides + personStats.driverCancelledRides),
          Se.Set BeamPS.completedRides (ps.completedRides + personStats.completedRides),
          Se.Set BeamPS.weekdayRides (ps.weekdayRides + personStats.weekdayRides),
          Se.Set BeamPS.weekendRides (ps.weekendRides + personStats.weekendRides),
          Se.Set BeamPS.offPeakRides (ps.offPeakRides + personStats.offPeakRides),
          Se.Set BeamPS.morningPeakRides (ps.morningPeakRides + personStats.morningPeakRides),
          Se.Set BeamPS.eveningPeakRides (ps.eveningPeakRides + personStats.eveningPeakRides),
          Se.Set BeamPS.weekendPeakRides (ps.weekendPeakRides + personStats.weekendPeakRides)
        ]
        [Se.Is BeamPS.personId (Se.Eq $ getId personStats.personId)]

findUserCancelledRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findUserCancelledRides (Id personId) = maybe (pure 0) (pure . Domain.userCancelledRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementUserCancelledRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementUserCancelledRidesCount (Id personId') = do
  now <- getCurrentTime
  findUserCancelledRides (Id personId') >>= \userCancelledRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> userCancelledRides) (userCancelledRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findDriverCancelledRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findDriverCancelledRides (Id personId) = maybe (pure 0) (pure . Domain.driverCancelledRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementDriverCancelledRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementDriverCancelledRidesCount (Id personId') = do
  now <- getCurrentTime
  findDriverCancelledRides (Id personId') >>= \driverCancelledRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> driverCancelledRides) (driverCancelledRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findCompletedRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findCompletedRides (Id personId) = maybe (pure 0) (pure . Domain.completedRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementCompletedRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementCompletedRidesCount (Id personId') = do
  now <- getCurrentTime
  findCompletedRides (Id personId') >>= \completedRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> completedRides) (completedRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findWeekendRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findWeekendRides (Id personId) = maybe (pure 0) (pure . Domain.weekendRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementWeekendRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementWeekendRidesCount (Id personId') = do
  now <- getCurrentTime
  findWeekendRides (Id personId') >>= \weekendRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> weekendRides) (weekendRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findWeekdayRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findWeekdayRides (Id personId) = maybe (pure 0) (pure . Domain.weekdayRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementWeekdayRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementWeekdayRidesCount (Id personId') = do
  now <- getCurrentTime
  findWeekdayRides (Id personId') >>= \weekdayRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> weekdayRides) (weekdayRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findOffPeakRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findOffPeakRides (Id personId) = maybe (pure 0) (pure . Domain.offPeakRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementOffpeakRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementOffpeakRidesCount (Id personId') = do
  now <- getCurrentTime
  findOffPeakRides (Id personId') >>= \offPeakRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> offPeakRides) (offPeakRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findEveningPeakRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findEveningPeakRides (Id personId) = maybe (pure 0) (pure . Domain.eveningPeakRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementEveningPeakRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementEveningPeakRidesCount (Id personId') = do
  now <- getCurrentTime
  findEveningPeakRides (Id personId') >>= \eveningPeakRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> eveningPeakRides) (eveningPeakRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findMorningPeakRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findMorningPeakRides (Id personId) = maybe (pure 0) (pure . Domain.morningPeakRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementMorningPeakRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementMorningPeakRidesCount (Id personId') = do
  now <- getCurrentTime
  findMorningPeakRides (Id personId') >>= \morningPeakRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> morningPeakRides) (morningPeakRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findWeekendPeakRides :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findWeekendPeakRides (Id personId) = maybe (pure 0) (pure . Domain.weekendPeakRides) =<< findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementWeekendPeakRidesCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m ()
incrementWeekendPeakRidesCount (Id personId') = do
  now <- getCurrentTime
  findWeekendPeakRides (Id personId') >>= \weekendPeakRidesCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> weekendPeakRides) (weekendPeakRidesCount + 1),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

instance FromTType' BeamPS.PersonStats Domain.PersonStats where
  fromTType' BeamPS.PersonStatsT {..} = do
    pure $
      Just $
        Domain.PersonStats
          { personId = Id personId,
            ..
          }

instance ToTType' BeamPS.PersonStats Domain.PersonStats where
  toTType' Domain.PersonStats {..} = do
    BeamPS.PersonStatsT
      { BeamPS.personId = getId personId,
        ..
      }
