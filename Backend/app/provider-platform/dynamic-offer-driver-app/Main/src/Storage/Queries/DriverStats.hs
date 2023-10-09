{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverStats where

import Control.Applicative (liftA2)
import Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import GHC.Float (double2Int, int2Double)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverStats as BeamDS

create :: MonadFlow m => DriverStats -> m ()
create = createWithKV

createInitialDriverStats :: MonadFlow m => Id Driver -> m ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  let dStats =
        DriverStats
          { driverId = driverId,
            idleSince = now,
            totalRides = 0,
            totalEarnings = 0,
            bonusEarned = 0,
            lateNightTrips = 0,
            earningsMissed = 0,
            totalDistance = 0,
            ridesCancelled = Just 0,
            totalRidesAssigned = Just 0,
            updatedAt = now
          }
  createWithKV dStats

updateIdleTime :: MonadFlow m => Id Driver -> m ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: MonadFlow m => [Id Driver] -> m ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDS.idleSince now
    ]
    [Se.Is BeamDS.driverId (Se.In (getId <$> driverIds))]

findById :: MonadFlow m => Id Driver -> m (Maybe DriverStats)
findById (Id driverId) = findOneWithKV [Se.Is BeamDS.driverId $ Se.Eq driverId]

deleteById :: MonadFlow m => Id Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

findTotalRides :: MonadFlow m => Id Driver -> m (Int, Meters)
findTotalRides (Id driverId) = maybe (pure (0, 0)) (pure . (Domain.totalRides &&& Domain.totalDistance)) =<< findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAndTotalDist :: (MonadFlow m) => Id Driver -> Meters -> m ()
incrementTotalRidesAndTotalDist (Id driverId') rideDist = do
  now <- getCurrentTime
  findTotalRides (Id driverId') >>= \(rides, distance) ->
    updateOneWithKV
      [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) (rides + 1),
        Se.Set BeamDS.totalDistance $ (\(Meters m) -> int2Double m) (rideDist + distance),
        Se.Set BeamDS.updatedAt now
      ]
      [Se.Is BeamDS.driverId (Se.Eq driverId')]

findTotalRidesAssigned :: MonadFlow m => Id Driver -> m (Maybe Int)
findTotalRidesAssigned (Id driverId) = (Domain.totalRidesAssigned =<<) <$> findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAssigned :: MonadFlow m => Id Driver -> Int -> m ()
incrementTotalRidesAssigned (Id driverId') number = do
  findTotalRidesAssigned (Id driverId') >>= \case
    Nothing -> updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just number)] [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Just newRides -> do
      updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just (newRides + number))] [Se.Is BeamDS.driverId (Se.Eq driverId')]

setCancelledRidesCount :: MonadFlow m => Id Driver -> Int -> m ()
setCancelledRidesCount (Id driverId') cancelledCount = updateOneWithKV [Se.Set BeamDS.ridesCancelled (Just cancelledCount)] [Se.Is BeamDS.driverId (Se.Eq driverId')]

setDriverStats :: (MonadFlow m) => Id Driver -> Int -> Int -> Money -> m ()
setDriverStats (Id driverId') totalRides cancelledCount missedEarning = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId')]
  case res of
    Nothing -> pure ()
    Just ds ->
      updateOneWithKV
        [ Se.Set BeamDS.updatedAt now,
          Se.Set BeamDS.totalRidesAssigned (liftA2 (+) (Just totalRides) ds.totalRidesAssigned),
          Se.Set BeamDS.ridesCancelled (Just cancelledCount),
          Se.Set BeamDS.earningsMissed missedEarning
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

setCancelledRidesCountAndIncrementEarningsMissed :: (MonadFlow m) => Id Driver -> Int -> Money -> m ()
setCancelledRidesCountAndIncrementEarningsMissed (Id driverId') cancelledCount missedEarning = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId')]
  case res of
    Nothing -> pure ()
    Just ds ->
      updateOneWithKV
        [ Se.Set BeamDS.updatedAt now,
          Se.Set BeamDS.ridesCancelled (Just cancelledCount),
          Se.Set BeamDS.earningsMissed (ds.earningsMissed + missedEarning)
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

incrementTotalEarningsAndBonusEarnedAndLateNightTrip :: (MonadFlow m) => Id Driver -> Money -> Money -> Int -> m ()
incrementTotalEarningsAndBonusEarnedAndLateNightTrip (Id driverId') increasedEarning increasedBonus tripCount = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId')]
  case res of
    Nothing -> pure ()
    Just ds ->
      updateOneWithKV
        [ Se.Set BeamDS.updatedAt now,
          Se.Set BeamDS.totalEarnings (ds.totalEarnings + increasedEarning),
          Se.Set BeamDS.bonusEarned (ds.bonusEarned + increasedBonus),
          Se.Set BeamDS.lateNightTrips (ds.lateNightTrips + tripCount)
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

setMissedEarnings :: (MonadFlow m) => Id Driver -> Money -> m ()
setMissedEarnings (Id driverId') missedEarnings = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDS.updatedAt now, Se.Set BeamDS.earningsMissed missedEarnings] [Se.Is BeamDS.driverId (Se.Eq driverId')]

instance FromTType' BeamDS.DriverStats DriverStats where
  fromTType' BeamDS.DriverStatsT {..} = do
    pure $
      Just
        DriverStats
          { driverId = Id driverId,
            idleSince = idleSince,
            totalRides = totalRides,
            totalDistance = Meters $ double2Int totalDistance,
            ridesCancelled = ridesCancelled,
            totalRidesAssigned = totalRidesAssigned,
            totalEarnings = totalEarnings,
            bonusEarned = bonusEarned,
            lateNightTrips = lateNightTrips,
            earningsMissed = earningsMissed,
            updatedAt = updatedAt
          }

instance ToTType' BeamDS.DriverStats DriverStats where
  toTType' DriverStats {..} = do
    BeamDS.DriverStatsT
      { BeamDS.driverId = getId driverId,
        BeamDS.idleSince = idleSince,
        BeamDS.totalRides = totalRides,
        BeamDS.totalDistance = (\(Meters m) -> int2Double m) totalDistance,
        BeamDS.ridesCancelled = ridesCancelled,
        BeamDS.totalRidesAssigned = totalRidesAssigned,
        BeamDS.totalEarnings = totalEarnings,
        BeamDS.bonusEarned = bonusEarned,
        BeamDS.lateNightTrips = lateNightTrips,
        BeamDS.earningsMissed = earningsMissed,
        BeamDS.updatedAt = updatedAt
      }
