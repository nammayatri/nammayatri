module Domain.Action.UI.DriverGamification where

import qualified API.Types.UI.DriverGamification as API
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
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BadgeDefinition as QBD
import qualified Storage.Queries.DriverBadge as QDB
import qualified Storage.Queries.DriverGamificationProgress as QGP
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.GamificationLevelConfig as QLC
import qualified Storage.Queries.Person as QP

-- GET /driver/gamification/journey
getDriverGamificationJourney ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.GamificationJourneyRes
  )
getDriverGamificationJourney (mbPersonId, merchantId, _merchantOpCityId) = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  person <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverStats <- B.runInReplica $ QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("DriverStats not found for " <> driverId.getId))
  progress <- B.runInReplica $ QGP.findByDriverId driverId
  levelConfigs <- B.runInReplica $ QLC.findAllByMerchantId merchantId
  allBadges <- B.runInReplica $ QBD.findAllActiveByMerchantId merchantId True
  earnedBadges <- B.runInReplica $ QDB.findAllByDriverId driverId

  let sortedLevels = sortBy (comparing GLC.displayOrder) levelConfigs
      totalXp = maybe 0 DGP.totalXp progress
      currentLevelName = maybe "BRONZE" DGP.currentLevel progress
      currentLevelInfo = findLevelInfo sortedLevels currentLevelName
      nextLevelInfo = findNextLevel sortedLevels totalXp
      xpToNext = case nextLevelInfo of
        Just nl -> GLC.minXp nl - totalXp
        Nothing -> 0

  pure $
    API.GamificationJourneyRes
      { currentLevel =
          API.GamificationLevelInfo
            { name = currentLevelName,
              iconUrl = currentLevelInfo >>= GLC.iconUrl,
              displayOrder = maybe 1 GLC.displayOrder currentLevelInfo
            },
        totalXp = totalXp,
        xpToNextLevel = xpToNext,
        nextLevel = GLC.levelName <$> nextLevelInfo,
        badgesEarned = length earnedBadges,
        totalBadges = length allBadges,
        currentStreakDays = maybe 0 DGP.currentStreakDays progress,
        longestStreakDays = maybe 0 DGP.longestStreakDays progress,
        lifetimeStats =
          API.LifetimeStatsInfo
            { totalRides = DDS.totalRides driverStats,
              totalEarnings = roundToIntegral $ DDS.totalEarnings driverStats,
              averageRating = (\r -> fromIntegral (round (r * 100) :: Int) / 100.0) . toRational <$> DDS.rating driverStats,
              memberSince = SP.createdAt person
            }
      }

-- GET /driver/gamification/badges
getDriverGamificationBadges ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Text ->
    Flow API.GamificationBadgesRes
  )
getDriverGamificationBadges (mbPersonId, merchantId, _merchantOpCityId) _mbCategory = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  driverStats <- B.runInReplica $ QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("DriverStats not found for " <> driverId.getId))
  allBadges <- B.runInReplica $ QBD.findAllActiveByMerchantId merchantId True
  earnedBadges <- B.runInReplica $ QDB.findAllByDriverId driverId
  let earnedMap = map (\db -> (DDB.badgeDefinitionId db, DDB.earnedAt db)) earnedBadges
  pure $
    API.GamificationBadgesRes
      { badges = map (toBadgeInfo earnedMap driverStats) allBadges
      }

-- GET /driver/gamification/milestones
getDriverGamificationMilestones ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.GamificationMilestonesRes
  )
getDriverGamificationMilestones (mbPersonId, merchantId, _merchantOpCityId) = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  driverStats <- B.runInReplica $ QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("DriverStats not found for " <> driverId.getId))
  allBadges <- B.runInReplica $ QBD.findAllActiveByMerchantId merchantId True
  earnedBadges <- B.runInReplica $ QDB.findAllByDriverId driverId
  let earnedIds = map DDB.badgeDefinitionId earnedBadges
      earnedMap = map (\db -> (DDB.badgeDefinitionId db, DDB.earnedAt db)) earnedBadges
      rideBadges = filter (\b -> DBD.criteriaType b == DBD.TOTAL_RIDES) allBadges
      milestones = map (toMilestoneInfo earnedIds earnedMap driverStats) rideBadges
  pure $
    API.GamificationMilestonesRes
      { milestones = milestones
      }

-- GET /driver/gamification/timeline
getDriverGamificationTimeline ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Int ->
    Maybe Int ->
    Flow API.GamificationTimelineRes
  )
getDriverGamificationTimeline (mbPersonId, _merchantId, _merchantOpCityId) mbLimit _mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  earnedBadges <- B.runInReplica $ QDB.findAllByDriverIdOrderByEarnedAt driverId (Just $ fromMaybe 10 mbLimit) (Just 0)
  let events = map toTimelineEvent earnedBadges
  pure $
    API.GamificationTimelineRes
      { events = events,
        totalCount = length events
      }

-- Helper functions

findLevelInfo :: [GLC.GamificationLevelConfig] -> Text -> Maybe GLC.GamificationLevelConfig
findLevelInfo levels levelName = find (\l -> GLC.levelName l == levelName) levels

findNextLevel :: [GLC.GamificationLevelConfig] -> Int -> Maybe GLC.GamificationLevelConfig
findNextLevel levels currentXp =
  let higherLevels = filter (\l -> GLC.minXp l > currentXp) levels
   in case sortBy (comparing GLC.minXp) higherLevels of
        (x : _) -> Just x
        [] -> Nothing

toBadgeInfo :: [(Id DBD.BadgeDefinition, UTCTime)] -> DDS.DriverStats -> DBD.BadgeDefinition -> API.GamificationBadgeInfo
toBadgeInfo earnedMap driverStats badge =
  let earned = lookup badge.id earnedMap
      currentVal = getCurrentValueForCriteria driverStats (DBD.criteriaType badge)
      prog = if isJust earned then Nothing else Just (min 100 $ (currentVal * 100) `div` max 1 (DBD.criteriaValue badge))
   in API.GamificationBadgeInfo
        { id = badge.id,
          name = DBD.name badge,
          description = DBD.description badge,
          iconUrl = DBD.iconUrl badge,
          category = show (DBD.category badge),
          tier = show (DBD.tier badge),
          isEarned = isJust earned,
          earnedAt = earned,
          criteriaType = show (DBD.criteriaType badge),
          criteriaValue = DBD.criteriaValue badge,
          progress = prog,
          xpReward = DBD.xpReward badge,
          coinReward = DBD.coinReward badge
        }

toMilestoneInfo :: [Id DBD.BadgeDefinition] -> [(Id DBD.BadgeDefinition, UTCTime)] -> DDS.DriverStats -> DBD.BadgeDefinition -> API.MilestoneInfo
toMilestoneInfo earnedIds earnedMap driverStats badge =
  let currentVal = getCurrentValueForCriteria driverStats (DBD.criteriaType badge)
      isAchieved = badge.id `elem` earnedIds
      prog = min 100 $ (currentVal * 100) `div` max 1 (DBD.criteriaValue badge)
   in API.MilestoneInfo
        { name = DBD.name badge,
          description = DBD.description badge,
          targetValue = DBD.criteriaValue badge,
          currentValue = currentVal,
          progressPercent = prog,
          isAchieved = isAchieved,
          achievedAt = lookup badge.id earnedMap,
          category = show (DBD.category badge)
        }

toTimelineEvent :: DDB.DriverBadge -> API.TimelineEvent
toTimelineEvent driverBadge =
  API.TimelineEvent
    { eventType = "BADGE_EARNED",
      title = "Badge Earned",
      description = "You earned a new badge!",
      iconUrl = Nothing,
      xpEarned = Nothing,
      coinsEarned = Nothing,
      timestamp = DDB.earnedAt driverBadge
    }

getCurrentValueForCriteria :: DDS.DriverStats -> DBD.BadgeCriteriaType -> Int
getCurrentValueForCriteria stats = \case
  DBD.TOTAL_RIDES -> DDS.totalRides stats
  DBD.TOTAL_EARNINGS -> roundToIntegral $ DDS.totalEarnings stats
  DBD.RATING_ABOVE -> maybe 0 (\r -> round (toRational r * 100)) (DDS.rating stats)
  DBD.STREAK_DAYS -> 0 -- Streak is tracked separately in DriverGamificationProgress
  DBD.LATE_NIGHT_TRIPS -> DDS.lateNightTrips stats
  DBD.FIVE_STAR_RATINGS -> fromMaybe 0 (DDS.totalRatingScore stats)
  DBD.REFERRALS -> DDS.totalReferralCounts stats
