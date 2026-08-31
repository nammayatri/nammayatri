module Lib.IncentiveJourney.Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
  )
where

import Data.List (find, nub)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), defaultTimeLocale, parseTimeM, utctDay)
import EulerHS.Prelude hiding (find, id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as IJC
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as API
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)

conditionOperatorOrDefault :: Maybe DIJM.MilestoneConditionOperator -> DIJM.MilestoneConditionOperator
conditionOperatorOrDefault = fromMaybe DIJM.GTE

buildSpecialLocationNames :: BeamFlow m r => ServiceHandle m -> [DIJM.IncentiveJourneyMilestone] -> m [(Text, Text)]
buildSpecialLocationNames handle milestones = do
  let locationIds =
        nub $
          concatMap
            (\milestone -> fromMaybe [] milestone.specialLocationIds)
            milestones
  case handle.findSpecialLocationNameById of
    Nothing -> pure [(lid, lid) | lid <- locationIds]
    Just findName -> do
      pairs <- forM locationIds $ \lid -> do
        mbName <- findName lid
        pure (lid, fromMaybe lid mbName)
      pure pairs

toSpecialLocationNames :: [(Text, Text)] -> Maybe [Text] -> Maybe [Text]
toSpecialLocationNames specialLocationNames =
  fmap (map resolveName)
  where
    resolveName locationId = maybe locationId snd (find ((== locationId) . fst) specialLocationNames)

resolveDisplayRewardValue :: DIJM.IncentiveJourneyMilestone -> Maybe Int
resolveDisplayRewardValue = (.rewardValue)

lookupAssignment :: [IJ.JourneyAssignment] -> Id DIJ.IncentiveJourney -> Maybe DCJM.CohortJourneyMapping
lookupAssignment assignments journeyId =
  (.cohortJourneyMapping) <$> find (\a -> a.cohortJourneyMapping.journeyId == journeyId) assignments

getIncentiveJourneyList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ( Maybe (Id IJC.Person),
    Id IJC.Merchant,
    Id IJC.MerchantOperatingCity
  ) ->
  Maybe Int ->
  Maybe Int ->
  m API.IncentiveJourneyListRes
getIncentiveJourneyList handle (mbPersonId, merchantId, merchantOpCityId) mbLimit mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  assignments <- handle.findAssignmentsByUserId driverId
  let mappedJourneyIds = map ((.journeyId) . (.cohortJourneyMapping)) assignments
  if null mappedJourneyIds
    then pure API.IncentiveJourneyListRes {journeys = []}
    else do
      allJourneys <- handle.getJourneys merchantOpCityId (Just $ cast merchantId) Nothing (Just True) Nothing
      let matching = filter (\j -> j.id `elem` mappedJourneyIds) allJourneys
          journeysWithMapping =
            [ (j, cjm)
              | j <- matching,
                Just cjm <- [lookupAssignment assignments j.id]
            ]
          orderedPairs = IJ.orderJourneyAssignmentPairsForDisplay localTime journeysWithMapping
          pagedPairs =
            take (fromMaybe (length orderedPairs) mbLimit)
              . drop (fromMaybe 0 mbOffset)
              $ orderedPairs
      if null pagedPairs
        then pure API.IncentiveJourneyListRes {journeys = []}
        else do
          journeys <-
            forM pagedPairs $ \(journey, cjm) -> do
              milestones <- handle.loadJourneyMilestones merchantOpCityId journey.id
              let periodKey = IJ.mkJourneyPeriodKeyFor localTime journey
                  endDate =
                    IJ.computeStreakEndDate
                      cjm.startDate
                      cjm.streakRange
                      (IJ.journeyTypeOrDefault journey.journeyType)
              statsRows <- handle.findStatsByPersonIdJourneyIdAndPeriodKey driverId journey.id periodKey
              specialLocationNames <- buildSpecialLocationNames handle milestones
              items <- mapM (toMilestoneItem specialLocationNames statsRows) milestones
              pure $
                API.IncentiveJourneyListItem
                  { journeyId = journey.id,
                    name = journey.name,
                    description = journey.description,
                    journeyType = journey.journeyType <|> Just DIJ.Daily,
                    startDate = cjm.startDate,
                    endDate = endDate,
                    streakRange = cjm.streakRange,
                    enabled = journey.enabled,
                    milestones = items
                  }
          pure API.IncentiveJourneyListRes {journeys = journeys}

toMilestoneItem :: BeamFlow m r => [(Text, Text)] -> [DIJS.IncentiveJourneyStats] -> DIJM.IncentiveJourneyMilestone -> m API.IncentiveJourneyMilestoneItem
toMilestoneItem specialLocationNames statsRows milestone = do
  let mbStats = find (\s -> s.milestoneId == milestone.id) statsRows
      displayRewardValue = resolveDisplayRewardValue milestone
  pure $
    API.IncentiveJourneyMilestoneItem
      { milestoneId = milestone.id,
        name = milestone.name,
        description = milestone.description,
        order = milestone.order,
        conditionType = milestone.conditionType,
        conditionOperator = conditionOperatorOrDefault milestone.conditionOperator,
        conditionValue = milestone.conditionValue,
        areaType = milestone.areaType,
        specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
        vehicleCategory = milestone.vehicleCategory,
        serviceTierType = milestone.serviceTierType,
        rewardType = milestone.rewardType,
        rewardValue = displayRewardValue,
        subscriptionWaiveOff =
          case milestone.rewardType of
            DIJC.SubscriptionWaiveOff ->
              (\spec -> API.SubscriptionWaiveOffConfig spec.percentage spec.daysValidFor spec.serviceName spec.waiveOffMode)
                <$> IJ.mkSubscriptionWaiveOffSpec
                  milestone.rewardValue
                  milestone.rewardExpirationAt
                  milestone.rewardMetadata
            _ -> Nothing,
        timeBounds = milestone.timeBounds,
        status = maybe DIJS.NotStarted (.status) mbStats,
        currentValue = maybe 0 (.currentValue) mbStats
      }

getIncentiveJourneyHistory ::
  BeamFlow m r =>
  ServiceHandle m ->
  ( Maybe (Id IJC.Person),
    Id IJC.Merchant,
    Id IJC.MerchantOperatingCity
  ) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m API.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory handle (mbPersonId, _merchantId, merchantOpCityId) mbDate mbLimit mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  let historyDay = fromMaybe (utctDay localTime) (mbDate >>= parseDateText)
      historyDayUtc = UTCTime historyDay 0
      dailyPeriodKey = IJ.mkDailyPeriodKey historyDayUtc
      weeklyPeriodKey = IJ.mkWeeklyPeriodKey historyDayUtc
      monthlyPeriodKey = IJ.mkMonthlyPeriodKey historyDayUtc
      limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dailyStatsRows <- handle.findStatsByPersonIdAndPeriodKey driverId dailyPeriodKey
  weeklyStatsRows <- handle.findStatsByPersonIdAndPeriodKey driverId weeklyPeriodKey
  monthlyStatsRows <- handle.findStatsByPersonIdAndPeriodKey driverId monthlyPeriodKey
  let dailyStatsIds = map (.id) dailyStatsRows
      weeklyOnly = filter (\stats -> stats.id `notElem` dailyStatsIds) weeklyStatsRows
      knownIds = dailyStatsIds <> map (.id) weeklyOnly
      monthlyOnly = filter (\stats -> stats.id `notElem` knownIds) monthlyStatsRows
      statsRows = dailyStatsRows <> weeklyOnly <> monthlyOnly
  let journeyIds = nub $ map (.journeyId) statsRows
  expanded <-
    concat
      <$> forM
        journeyIds
        ( \journeyId -> do
            mbJourney <- handle.getOneJourney merchantOpCityId journeyId
            milestones <- handle.loadJourneyMilestones merchantOpCityId journeyId
            specialLocationNames <- buildSpecialLocationNames handle milestones
            let journeyName = maybe journeyId.getId (.name) mbJourney
                journeyType = (mbJourney >>= (.journeyType)) <|> Just DIJ.Daily
                journeyStats = filter (\s -> s.journeyId == journeyId) statsRows
                defaultPeriodKey = maybe ("Day:" <> T.pack (show historyDay)) (.periodKey) (listToMaybe journeyStats)
            forM milestones $ \milestone -> do
              let mbStats = find (\s -> s.milestoneId == milestone.id) journeyStats
                  displayRewardValue = resolveDisplayRewardValue milestone
              pure $ mkHistoryItem specialLocationNames journeyId journeyName journeyType milestone defaultPeriodKey displayRewardValue mbStats
        )
  let history =
        take limitVal
          . drop offsetVal
          . sortOn (\item -> (item.journeyName, item.milestoneOrder))
          $ expanded
  pure API.IncentiveJourneyHistoryRes {history = history}

mkHistoryItem ::
  [(Text, Text)] ->
  Id DIJ.IncentiveJourney ->
  Text ->
  Maybe DIJ.IncentiveJourneyType ->
  DIJM.IncentiveJourneyMilestone ->
  Text ->
  Maybe Int ->
  Maybe DIJS.IncentiveJourneyStats ->
  API.IncentiveJourneyHistoryItem
mkHistoryItem specialLocationNames journeyId journeyName journeyType milestone defaultPeriodKey displayRewardValue mbStats =
  case mbStats of
    Just stats ->
      API.IncentiveJourneyHistoryItem
        { journeyId = journeyId,
          journeyName = journeyName,
          journeyType = journeyType,
          milestoneId = milestone.id,
          milestoneName = milestone.name,
          milestoneDescription = milestone.description,
          milestoneOrder = milestone.order,
          conditionType = stats.conditionType,
          conditionOperator = conditionOperatorOrDefault stats.conditionOperator,
          conditionValue = stats.conditionValue,
          areaType = milestone.areaType,
          specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
          currentValue = stats.currentValue,
          status = stats.status,
          rewardType = stats.rewardType,
          rewardValue = displayRewardValue,
          periodKey = stats.periodKey,
          completedAt = completedAtForStatus stats.status stats.updatedAt
        }
    Nothing ->
      API.IncentiveJourneyHistoryItem
        { journeyId = journeyId,
          journeyName = journeyName,
          journeyType = journeyType,
          milestoneId = milestone.id,
          milestoneName = milestone.name,
          milestoneDescription = milestone.description,
          milestoneOrder = milestone.order,
          conditionType = milestone.conditionType,
          conditionOperator = conditionOperatorOrDefault milestone.conditionOperator,
          conditionValue = milestone.conditionValue,
          areaType = milestone.areaType,
          specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
          currentValue = 0,
          status = DIJS.NotStarted,
          rewardType = milestone.rewardType,
          rewardValue = displayRewardValue,
          periodKey = defaultPeriodKey,
          completedAt = Nothing
        }

completedAtForStatus :: DIJS.JourneyMilestoneStatus -> UTCTime -> Maybe UTCTime
completedAtForStatus status updatedAt =
  case status of
    DIJS.Completed -> Just updatedAt
    DIJS.Rewarded -> Just updatedAt
    DIJS.WaivedOff -> Just updatedAt
    _ -> Nothing

parseDateText :: Text -> Maybe Day
parseDateText =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack
