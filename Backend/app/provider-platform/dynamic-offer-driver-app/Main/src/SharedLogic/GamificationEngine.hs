module SharedLogic.GamificationEngine where

import qualified Domain.Types.BadgeDefinition as DBD
import qualified Domain.Types.DriverBadge as DDB
import qualified Domain.Types.DriverGamificationProgress as DGP
import qualified Domain.Types.DriverStats as DDS
import qualified Domain.Types.GamificationLevelConfig as GLC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BadgeDefinition as QBD
import qualified Storage.Queries.DriverBadge as QDB
import qualified Storage.Queries.DriverGamificationProgress as QGP
import qualified Storage.Queries.GamificationLevelConfig as QLC

-- | Called after ride completion to check and award any newly earned badges.
-- Runs asynchronously (via fork) to avoid adding latency to the ride completion flow.
checkAndAwardBadges ::
  Id SP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DDS.DriverStats ->
  Flow ()
checkAndAwardBadges driverId merchantId merchantOpCityId driverStats = do
  allBadges <- B.runInReplica $ QBD.findAllActiveByMerchantId merchantId True
  earnedBadges <- B.runInReplica $ QDB.findAllByDriverId driverId
  let earnedIds = map DDB.badgeDefinitionId earnedBadges
      unearnedBadges = filter (\b -> b.id `notElem` earnedIds) allBadges
  now <- getCurrentTime
  forM_ unearnedBadges $ \badge -> do
    let currentVal = getCurrentValueForCriteria driverStats (DBD.criteriaType badge)
    when (currentVal >= DBD.criteriaValue badge) $ do
      newId <- generateGUID
      let newDriverBadge =
            DDB.DriverBadge
              { id = newId,
                driverId = driverId,
                badgeDefinitionId = badge.id,
                earnedAt = now,
                merchantOperatingCityId = merchantOpCityId
              }
      QDB.create newDriverBadge
      awardXP driverId merchantId (DBD.xpReward badge)
      -- Update badges earned count
      progress <- QGP.findByDriverId driverId
      case progress of
        Just p -> QGP.updateBadgesEarnedCount (DGP.badgesEarnedCount p + 1) now driverId
        Nothing -> do
          QGP.create $
            DGP.DriverGamificationProgress
              { driverId = driverId,
                totalXp = DBD.xpReward badge,
                currentLevel = "BRONZE",
                currentStreakDays = 0,
                longestStreakDays = 0,
                lastRideDate = Nothing,
                badgesEarnedCount = 1,
                merchantOperatingCityId = merchantOpCityId,
                createdAt = now,
                updatedAt = now
              }
      logInfo $ "Awarded badge " <> DBD.name badge <> " to driver " <> driverId.getId

-- | Calculate and update driver level based on total XP.
calculateDriverLevel ::
  Id SP.Person ->
  Id DM.Merchant ->
  Flow ()
calculateDriverLevel driverId merchantId = do
  progress <- QGP.findByDriverId driverId
  case progress of
    Nothing -> pure ()
    Just p -> do
      levelConfigs <- B.runInReplica $ QLC.findAllByMerchantId merchantId
      let sortedLevels = sortBy (flip $ comparing GLC.minXp) levelConfigs
          newLevel = case find (\l -> DGP.totalXp p >= GLC.minXp l) sortedLevels of
            Just l -> GLC.levelName l
            Nothing -> "BRONZE"
      when (newLevel /= DGP.currentLevel p) $ do
        now <- getCurrentTime
        QGP.updateTotalXpAndLevel (DGP.totalXp p) newLevel now driverId
        logInfo $ "Driver " <> driverId.getId <> " leveled up to " <> newLevel

-- | Check and update ride streak for a driver.
-- Should be called after each ride completion.
checkStreak ::
  Id SP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Day ->
  Flow ()
checkStreak driverId merchantOpCityId today = do
  progress <- QGP.findByDriverId driverId
  now <- getCurrentTime
  case progress of
    Nothing -> do
      QGP.create $
        DGP.DriverGamificationProgress
          { driverId = driverId,
            totalXp = 0,
            currentLevel = "BRONZE",
            currentStreakDays = 1,
            longestStreakDays = 1,
            lastRideDate = Just today,
            badgesEarnedCount = 0,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
    Just p -> do
      let lastDate = DGP.lastRideDate p
          (newStreak, newLongest) = case lastDate of
            Just ld
              | ld == today -> (DGP.currentStreakDays p, DGP.longestStreakDays p) -- Same day, no change
              | ld == addDays (-1) today -> -- Consecutive day
                  let s = DGP.currentStreakDays p + 1
                   in (s, max s (DGP.longestStreakDays p))
              | otherwise -> (1, DGP.longestStreakDays p) -- Streak broken
            Nothing -> (1, max 1 (DGP.longestStreakDays p))
      QGP.updateStreakDays newStreak newLongest (Just today) now driverId

-- | Award XP to a driver and recalculate level.
awardXP ::
  Id SP.Person ->
  Id DM.Merchant ->
  Int ->
  Flow ()
awardXP driverId merchantId xpAmount = do
  when (xpAmount > 0) $ do
    progress <- QGP.findByDriverId driverId
    now <- getCurrentTime
    case progress of
      Just p -> do
        let newXp = DGP.totalXp p + xpAmount
        QGP.updateTotalXpAndLevel newXp (DGP.currentLevel p) now driverId
      Nothing -> pure () -- Progress should already be created by checkAndAwardBadges or checkStreak
    calculateDriverLevel driverId merchantId

-- | Get the current value of a specific criteria from driver stats.
getCurrentValueForCriteria :: DDS.DriverStats -> DBD.BadgeCriteriaType -> Int
getCurrentValueForCriteria stats = \case
  DBD.TOTAL_RIDES -> DDS.totalRides stats
  DBD.TOTAL_EARNINGS -> roundToIntegral $ DDS.totalEarnings stats
  DBD.RATING_ABOVE -> maybe 0 (\r -> round (toRational r * 100)) (DDS.rating stats)
  DBD.STREAK_DAYS -> 0
  DBD.LATE_NIGHT_TRIPS -> DDS.lateNightTrips stats
  DBD.FIVE_STAR_RATINGS -> fromMaybe 0 (DDS.totalRatingScore stats)
  DBD.REFERRALS -> DDS.totalReferralCounts stats
