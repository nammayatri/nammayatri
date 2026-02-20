module Storage.Queries.DriverStatsExtra where

import Control.Applicative (liftA2)
import Domain.Types.DriverInformation
import Domain.Types.DriverStats as Domain
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Vehicle
import GHC.Float (int2Double)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import SharedLogic.DriverFee (mkCachedKeyTotalRidesByDriverId)
import qualified Storage.Beam.DriverStats as BeamDS
import Storage.Queries.OrphanInstances.DriverStats ()
import Storage.Queries.Person (DriverWithRidesCount (..), fetchDriverInfo)
import Tools.Error

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
            favRiderCount = 0,
            rating = Nothing,
            totalRatings = Just 0,
            totalRatingScore = Just 0,
            isValidRating = Just False,
            totalPayoutEarnings = 0.0,
            totalPayoutAmountPaid = Nothing,
            totalValidActivatedRides = 0,
            totalReferralCounts = 0,
            updatedAt = now,
            validCustomerCancellationTagCount = 0,
            validDriverCancellationTagCount = 0,
            validCancellationTagsStatsStartDate = Just now,
            numDriversOnboarded = 0,
            numFleetsOnboarded = 0,
            safetyPlusEarnings = 0.0,
            safetyPlusRideCount = 0,
            coinsConvertedToDirectPayoutCash = 0.0,
            onlineDuration = Seconds 0,
            blacklistCoinEvents = Nothing
          }
  createWithKV dStats

getTopDriversByIdleTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids = findAllWithOptionsDb [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing <&> (Domain.driverId <$>)

updateIdleTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m ()
updateIdleTime (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDS.idleSince now
    ]
    [Se.Is BeamDS.driverId (Se.Eq driverId)]

fetchAll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [DriverStats]
fetchAll = findAllWithKV [Se.Is BeamDS.driverId $ Se.Not $ Se.Eq $ getId ""]

findAllByDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Driver] -> m [DriverStats]
findAllByDriverIds person = findAllWithKV [Se.Is BeamDS.driverId $ Se.In (getId <$> (person <&> (.id)))]

incrementTotalRidesAndTotalDistAndIdleTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) => Id Driver -> Meters -> m Int
incrementTotalRidesAndTotalDistAndIdleTime (Id driverId') rideDist = do
  now <- getCurrentTime
  newTotalRides <-
    findTotalRides (Id driverId') >>= \(rides, distance) -> do
      updateOneWithKV
        [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) (rides + 1),
          Se.Set BeamDS.totalDistance $ (\(Meters m) -> int2Double m) (rideDist + distance),
          Se.Set BeamDS.idleSince now,
          Se.Set BeamDS.updatedAt now
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]
      totalRideKey :: (Maybe Int) <- Redis.safeGet $ mkCachedKeyTotalRidesByDriverId (Id driverId')
      case totalRideKey of
        Nothing -> Redis.setExp (mkCachedKeyTotalRidesByDriverId (Id driverId')) (rides + 1) 86400
        Just _ -> void $ Redis.incr (mkCachedKeyTotalRidesByDriverId (Id driverId'))
      pure (rides + 1)
  pure newTotalRides

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

updateCoinsFieldsForDirectPayout :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> m ()
updateCoinsFieldsForDirectPayout driverId amount = do
  now <- getCurrentTime
  mbDriverStat <- findById driverId
  case mbDriverStat of
    Just driverStat -> do
      updateWithKV
        [ Se.Set BeamDS.totalCoinsConvertedCash $ Just (driverStat.totalCoinsConvertedCash + amount),
          Se.Set BeamDS.coinsConvertedToDirectPayoutCash $ Just (driverStat.coinsConvertedToDirectPayoutCash + amount),
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

decFavouriteRiderCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m ()
decFavouriteRiderCount driverId = do
  mbDriverDetail <- findById driverId
  case mbDriverDetail of
    Just driverDetail -> updateOneWithKV [Se.Set BeamDS.favRiderCount (driverDetail.favRiderCount - 1)] [Se.Is BeamDS.driverId (Se.Eq driverId.getId)]
    Nothing -> pure ()

incFavouriteRiderCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m ()
incFavouriteRiderCount driverId = do
  mbDriverDetail <- findById driverId
  case mbDriverDetail of
    Just driverDetail -> updateOneWithKV [Se.Set BeamDS.favRiderCount (driverDetail.favRiderCount + 1)] [Se.Is BeamDS.driverId (Se.Eq driverId.getId)]
    Nothing -> pure ()

updateAverageRating :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Maybe Int -> Maybe Int -> Maybe Bool -> m ()
updateAverageRating (Id driverId') totalRatingsCount' totalRatingScore' isValidRating' = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRatings) totalRatingsCount',
      Se.Set (\BeamDS.DriverStatsT {..} -> totalRatingScore) totalRatingScore',
      Se.Set BeamDS.isValidRating isValidRating',
      Se.Set BeamDS.updatedAt now
    ]
    [Se.Is BeamDS.driverId (Se.Eq driverId')]

fetchDriverInfoWithRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> Maybe Text -> Maybe (Id Person) -> m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchant moCity mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash email mbDriverId = do
  mbDriverInfo <- fetchDriverInfo merchant moCity mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash email mbDriverId
  addRidesCount `mapM` mbDriverInfo
  where
    addRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Person, DriverInformation, Maybe Vehicle) -> m DriverWithRidesCount
    addRidesCount (person, info, vehicle) = do
      driverStats <- findById person.id >>= fromMaybeM DriverInfoNotFound
      let ridesCount = Just driverStats.totalRides
      pure (mkDriverWithRidesCount (person, info, vehicle, ridesCount))

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

incSafetyPlusRiderCountAndEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> HighPrecMoney -> m ()
incSafetyPlusRiderCountAndEarnings driverId safetyPlusEarnings = do
  mbDriverDetail <- findById driverId
  case mbDriverDetail of
    Just driverDetail -> do
      updateOneWithKV
        [ Se.Set BeamDS.safetyPlusRideCount $ Just (driverDetail.safetyPlusRideCount + 1),
          Se.Set BeamDS.safetyPlusEarnings $ Just (driverDetail.safetyPlusEarnings + safetyPlusEarnings)
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId.getId)]
    Nothing -> pure ()
