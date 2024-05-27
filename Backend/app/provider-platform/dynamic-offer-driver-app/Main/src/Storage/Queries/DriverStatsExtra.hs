{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverStatsExtra where

import Control.Applicative (liftA2)
import Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import GHC.Float (double2Int, int2Double)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverStats as BeamDS
import Storage.Queries.OrphanInstances.DriverStats

-- Extra code goes here --

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Maybe DriverStats)
findById (Id driverId) = findOneWithKV [Se.Is BeamDS.driverId $ Se.Eq driverId]

createInitialDriverStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Currency -> DistanceUnit -> Id Driver -> m ()
createInitialDriverStats currency distanceUnit driverId = do
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
            distanceUnit,
            updatedAt = now,
            favRiderCount = 0,
            favRiderList = []
          }
  createWithKV dStats

getTopDriversByIdleTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids = findAllWithOptionsDb [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing <&> (Domain.driverId <$>)

updateIdleTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Driver] -> m ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDS.idleSince now
    ]
    [Se.Is BeamDS.driverId (Se.In (getId <$> driverIds))]

fetchAll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [DriverStats]
fetchAll = findAllWithKV [Se.Is BeamDS.driverId $ Se.Not $ Se.Eq $ getId ""]

incrementTotalRidesAndTotalDist :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Meters -> m ()
incrementTotalRidesAndTotalDist (Id driverId') rideDist = do
  now <- getCurrentTime
  findTotalRides (Id driverId') >>= \(rides, distance) ->
    updateOneWithKV
      [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) (rides + 1),
        Se.Set BeamDS.totalDistance $ (\(Meters m) -> int2Double m) (rideDist + distance),
        Se.Set BeamDS.updatedAt now
      ]
      [Se.Is BeamDS.driverId (Se.Eq driverId')]

findTotalRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Int, Meters)
findTotalRides (Id driverId) = maybe (pure (0, 0)) (pure . (Domain.totalRides &&& Domain.totalDistance)) =<< findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

findTotalRidesAssigned :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Maybe Int)
findTotalRidesAssigned (Id driverId) = (Domain.totalRidesAssigned =<<) <$> findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAssigned :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Int -> m ()
incrementTotalRidesAssigned (Id driverId') number = do
  findTotalRidesAssigned (Id driverId') >>= \case
    Nothing -> updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just number)] [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Just newRides -> do
      updateOneWithKV [Se.Set BeamDS.totalRidesAssigned (Just (newRides + number))] [Se.Is BeamDS.driverId (Se.Eq driverId')]

setDriverStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Int -> Int -> HighPrecMoney -> m ()
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

getDriversSortedOrder :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Integer -> m [DriverStats]
getDriversSortedOrder mbLimitVal = findAllWithOptionsDb [] (Se.Desc BeamDS.totalRides) (Just $ maybe 10 fromInteger mbLimitVal) Nothing

setCancelledRidesCountAndIncrementEarningsMissed :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Int -> HighPrecMoney -> m ()
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

incrementTotalEarningsAndBonusEarnedAndLateNightTrip :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> HighPrecMoney -> Int -> m ()
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

updateCoinToCashByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> m ()
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

updateCoinFieldsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> m ()
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

setMissedEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> m ()
setMissedEarnings (Id driverId') missedEarnings = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDS.updatedAt now,
      Se.Set BeamDS.earningsMissedAmount $ Just missedEarnings,
      Se.Set BeamDS.earningsMissed $ roundToIntegral missedEarnings
    ]
    [Se.Is BeamDS.driverId (Se.Eq driverId')]

addToFavRiderList :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Driver -> m ()
addToFavRiderList riderId (Id driverId) = do
  mbDriverDetail <- findById (Id driverId)
  case mbDriverDetail of
    Just driverDetail -> do
      let favRiderList = driverDetail.favRiderList
      unless (riderId `elem` favRiderList) $ do
        updateOneWithKV
          [ Se.Set BeamDS.favRiderList (favRiderList <> [riderId]),
            Se.Set BeamDS.favRiderCount (driverDetail.favRiderCount + 1)
          ]
          [Se.Is BeamDS.driverId (Se.Eq driverId)]
    Nothing -> do
      logError $ "Driver stats doesn't exist for driver Id : " <> driverId
      pure ()

removeFavouriteRider :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Text -> m ()
removeFavouriteRider (Id driverId) riderId = do
  mbDriverDetail <- findById (Id driverId)
  case mbDriverDetail of
    Just driverDetail -> do
      let favRiderList = driverDetail.favRiderList
          newFavRiderList = filter (/= riderId) favRiderList
      when (riderId `elem` favRiderList) $ do
        updateOneWithKV
          [ Se.Set BeamDS.favRiderList newFavRiderList,
            Se.Set BeamDS.favRiderCount (driverDetail.favRiderCount - 1)
          ]
          [Se.Is BeamDS.driverId (Se.Eq driverId)]
    Nothing -> pure ()
