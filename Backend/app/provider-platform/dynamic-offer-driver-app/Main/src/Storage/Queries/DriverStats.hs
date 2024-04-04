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

create :: KvDbFlow m r => DriverStats -> m ()
create = createWithKV

createInitialDriverStats :: KvDbFlow m r => Currency -> Id Driver -> m ()
createInitialDriverStats currency driverId = do
  now <- getCurrentTime
  let dStats =
        DriverStats
          { driverId = driverId,
            idleSince = now,
            totalRides = 0,
            totalEarnings = 0.0,
            bonusEarned = 0.0,
            lateNightTrips = 0,
            earningsMissed = 0.0,
            totalDistance = 0,
            ridesCancelled = Just 0,
            totalRidesAssigned = Just 0,
            coinCovertedToCashLeft = 0.0,
            totalCoinsConvertedCash = 0.0,
            currency,
            updatedAt = now
          }
  createWithKV dStats

getTopDriversByIdleTime :: KvDbFlow m r => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids = findAllWithOptionsDb [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing <&> (Domain.driverId <$>)

updateIdleTime :: KvDbFlow m r => Id Driver -> m ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: KvDbFlow m r => [Id Driver] -> m ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDS.idleSince now
    ]
    [Se.Is BeamDS.driverId (Se.In (getId <$> driverIds))]

fetchAll :: KvDbFlow m r => m [DriverStats]
fetchAll = findAllWithKV [Se.Is BeamDS.driverId $ Se.Not $ Se.Eq $ getId ""]

findById :: KvDbFlow m r => Id Driver -> m (Maybe DriverStats)
findById (Id driverId) = findOneWithKV [Se.Is BeamDS.driverId $ Se.Eq driverId]

deleteById :: KvDbFlow m r => Id Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

findTotalRides :: KvDbFlow m r => Id Driver -> m (Int, Meters)
findTotalRides (Id driverId) = maybe (pure (0, 0)) (pure . (Domain.totalRides &&& Domain.totalDistance)) =<< findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAndTotalDist :: KvDbFlow m r => Id Driver -> Meters -> m ()
incrementTotalRidesAndTotalDist (Id driverId') rideDist = do
  now <- getCurrentTime
  findTotalRides (Id driverId') >>= \(rides, distance) ->
    updateOneWithKV
      [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) (rides + 1),
        Se.Set BeamDS.totalDistance $ (\(Meters m) -> int2Double m) (rideDist + distance),
        Se.Set BeamDS.updatedAt now
      ]
      [Se.Is BeamDS.driverId (Se.Eq driverId')]

findTotalRidesAssigned :: KvDbFlow m r => Id Driver -> m (Maybe Int)
findTotalRidesAssigned (Id driverId) = (Domain.totalRidesAssigned =<<) <$> findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAssigned :: KvDbFlow m r => Id Driver -> Int -> m ()
incrementTotalRidesAssigned (Id driverId') number = do
  findTotalRidesAssigned (Id driverId') >>= \case
    Nothing -> updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just number)] [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Just newRides -> do
      updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just (newRides + number))] [Se.Is BeamDS.driverId (Se.Eq driverId')]

setCancelledRidesCount :: KvDbFlow m r => Id Driver -> Int -> m ()
setCancelledRidesCount (Id driverId') cancelledCount = updateOneWithKV [Se.Set BeamDS.ridesCancelled (Just cancelledCount)] [Se.Is BeamDS.driverId (Se.Eq driverId')]

setDriverStats :: KvDbFlow m r => Id Driver -> Int -> Int -> HighPrecMoney -> m ()
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
          Se.Set BeamDS.earningsMissed $ roundToIntegral missedEarning,
          Se.Set BeamDS.earningsMissedAmount $ Just missedEarning
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

getDriversSortedOrder :: KvDbFlow m r => Maybe Integer -> m [DriverStats]
getDriversSortedOrder mbLimitVal = findAllWithOptionsDb [] (Se.Desc BeamDS.totalRides) (Just $ maybe 10 fromInteger mbLimitVal) Nothing

setCancelledRidesCountAndIncrementEarningsMissed :: KvDbFlow m r => Id Driver -> Int -> HighPrecMoney -> m ()
setCancelledRidesCountAndIncrementEarningsMissed (Id driverId') cancelledCount missedEarning = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId')]
  case res of
    Nothing -> pure ()
    Just ds ->
      updateOneWithKV
        [ Se.Set BeamDS.updatedAt now,
          Se.Set BeamDS.ridesCancelled (Just cancelledCount),
          Se.Set BeamDS.earningsMissed $ roundToIntegral (ds.earningsMissed + missedEarning),
          Se.Set BeamDS.earningsMissedAmount $ Just (ds.earningsMissed + missedEarning)
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

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
            totalEarnings = mkAmountWithDefault totalEarningsAmount totalEarnings,
            bonusEarned = mkAmountWithDefault bonusEarnedAmount bonusEarned,
            lateNightTrips = lateNightTrips,
            earningsMissed = mkAmountWithDefault earningsMissedAmount earningsMissed,
            coinCovertedToCashLeft = fromMaybe 0 coinCovertedToCashLeft,
            totalCoinsConvertedCash = fromMaybe 0 totalCoinsConvertedCash,
            currency = fromMaybe INR currency,
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
        BeamDS.totalEarnings = roundToIntegral totalEarnings,
        BeamDS.currency = Just currency,
        BeamDS.bonusEarned = roundToIntegral bonusEarned,
        BeamDS.totalEarningsAmount = Just totalEarnings,
        BeamDS.bonusEarnedAmount = Just bonusEarned,
        BeamDS.lateNightTrips = lateNightTrips,
        BeamDS.earningsMissed = roundToIntegral earningsMissed,
        BeamDS.earningsMissedAmount = Just earningsMissed,
        BeamDS.coinCovertedToCashLeft = Just coinCovertedToCashLeft,
        BeamDS.totalCoinsConvertedCash = Just totalCoinsConvertedCash,
        BeamDS.updatedAt = updatedAt
      }

incrementTotalEarningsAndBonusEarnedAndLateNightTrip :: KvDbFlow m r => Id Driver -> HighPrecMoney -> HighPrecMoney -> Int -> m ()
incrementTotalEarningsAndBonusEarnedAndLateNightTrip (Id driverId') increasedEarning increasedBonus tripCount = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId')]
  case res of
    Nothing -> pure ()
    Just ds ->
      updateOneWithKV
        [ Se.Set BeamDS.updatedAt now,
          Se.Set BeamDS.totalEarnings $ roundToIntegral (ds.totalEarnings + increasedEarning),
          Se.Set BeamDS.totalEarningsAmount $ Just (ds.totalEarnings + increasedEarning),
          Se.Set BeamDS.bonusEarned $ roundToIntegral (ds.bonusEarned + increasedBonus),
          Se.Set BeamDS.bonusEarnedAmount $ Just (ds.bonusEarned + increasedBonus),
          Se.Set BeamDS.lateNightTrips (ds.lateNightTrips + tripCount)
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]

updateCoinToCashByDriverId :: KvDbFlow m r => Id Driver -> HighPrecMoney -> m ()
updateCoinToCashByDriverId driverId amountToAdd = do
  now <- getCurrentTime
  mbDriverStat <- findById driverId
  case mbDriverStat of
    Just driverStat -> do
      updateWithKV
        [ Se.Set BeamDS.coinCovertedToCashLeft $ Just (driverStat.coinCovertedToCashLeft + amountToAdd),
          Se.Set BeamDS.updatedAt now
        ]
        [Se.Is BeamDS.driverId (Se.Eq (getId driverId))]
    Nothing -> pure ()

updateCoinFieldsByDriverId :: KvDbFlow m r => Id Driver -> HighPrecMoney -> m ()
updateCoinFieldsByDriverId driverId amount = do
  now <- getCurrentTime
  mbDriverStat <- findById driverId
  case mbDriverStat of
    Just driverStat -> do
      updateWithKV
        [ Se.Set BeamDS.coinCovertedToCashLeft $ Just (driverStat.coinCovertedToCashLeft + amount),
          Se.Set BeamDS.totalCoinsConvertedCash $ Just (driverStat.totalCoinsConvertedCash + amount),
          Se.Set BeamDS.updatedAt now
        ]
        [Se.Is BeamDS.driverId (Se.Eq (getId driverId))]
    Nothing -> pure ()

setMissedEarnings :: KvDbFlow m r => Id Driver -> HighPrecMoney -> m ()
setMissedEarnings (Id driverId') missedEarnings = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDS.updatedAt now,
      Se.Set BeamDS.earningsMissedAmount $ Just missedEarnings,
      Se.Set BeamDS.earningsMissed $ roundToIntegral missedEarnings
    ]
    [Se.Is BeamDS.driverId (Se.Eq driverId')]
