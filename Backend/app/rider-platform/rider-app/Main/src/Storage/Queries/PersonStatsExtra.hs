module Storage.Queries.PersonStatsExtra where

import Domain.Types.Person
import qualified Domain.Types.PersonStats as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.PersonStats as BeamPS
import Storage.Queries.OrphanInstances.PersonStats ()

createPersonStats :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.PersonStats -> m Domain.PersonStats
createPersonStats p = createWithKV p >> pure p

incrementOrSetPersonStats :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.PersonStats -> m ()
incrementOrSetPersonStats personStats = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamPS.personId (Se.Eq (getId personStats.personId))]
  whenJust res $ \ps ->
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

findPersonStats :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Domain.PersonStats)
findPersonStats (Id personId) = findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]

incrementCompletedRidesEventCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Bool -> Bool -> Bool -> Bool -> Bool -> m ()
incrementCompletedRidesEventCount (Id personId') isWeekend isMorningPeak isEveningPeak isWeekendPeak isAnyPeak = do
  now <- getCurrentTime
  -- Case analysis for the Bool type. bool f t p evaluates to f when p is False, and evaluates to t when p is True.
  mbPersonStats <- findPersonStats (Id personId')
  whenJust mbPersonStats $ \Domain.PersonStats {..} ->
    updateOneWithKV
      [ Se.Set BeamPS.completedRides (completedRides + 1),
        Se.Set BeamPS.weekdayRides (weekdayRides + bool 0 1 (not isWeekend)),
        Se.Set BeamPS.weekendRides (weekendRides + bool 0 1 isWeekend),
        Se.Set BeamPS.offPeakRides (offPeakRides + bool 0 1 (not isAnyPeak)),
        Se.Set BeamPS.morningPeakRides (morningPeakRides + bool 0 1 isMorningPeak),
        Se.Set BeamPS.eveningPeakRides (eveningPeakRides + bool 0 1 isEveningPeak),
        Se.Set BeamPS.weekendPeakRides (weekendPeakRides + bool 0 1 isWeekendPeak),
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

incrementTicketsBookedInEvent :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Int -> m ()
incrementTicketsBookedInEvent (Id personId') newTicketsCount = do
  now <- getCurrentTime
  findTicketsBookedInEvent (Id personId') >>= \ticketsBookedInEventCount ->
    updateOneWithKV
      [ Se.Set (\BeamPS.PersonStatsT {..} -> ticketsBookedInEvent) (Just $ ticketsBookedInEventCount + newTicketsCount),
        Se.Set BeamPS.updatedAt now
      ]
      [Se.Is BeamPS.personId (Se.Eq personId')]

findTicketsBookedInEvent :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
findTicketsBookedInEvent (Id personId) = do
  stats <- findOneWithKV [Se.Is BeamPS.personId (Se.Eq personId)]
  pure $ fromMaybe 0 ((.ticketsBookedInEvent) =<< stats)
