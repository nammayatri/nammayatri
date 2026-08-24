module Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
  )
where

import qualified API.Types.UI.IncentiveJourney as API
import Data.List (find, nub)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), defaultTimeLocale, parseTimeM, utctDay)
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.IncentiveJourneyStats as DIJS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.VehicleVariant as VecVariant
import Environment
import EulerHS.Prelude hiding (find, id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified SharedLogic.IncentiveJourney as SLJourney
import Storage.Beam.SpecialZone ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyStats as CQStats
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.IncentiveJourneyStats as QStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

conditionOperatorOrDefault :: Maybe DIJM.MilestoneConditionOperator -> DIJM.MilestoneConditionOperator
conditionOperatorOrDefault = fromMaybe DIJM.GTE

buildSpecialLocationNames :: [DIJM.IncentiveJourneyMilestone] -> Flow [(Text, Text)]
buildSpecialLocationNames milestones = do
  let locationIds =
        nub $
          concatMap
            (\milestone -> fromMaybe [] milestone.pickupSpecialLocationIds <> fromMaybe [] milestone.dropSpecialLocationIds)
            milestones
  specialLocations <- mapM (QSpecialLocation.findById . Id) locationIds
  pure [(specialLocation.id.getId, specialLocation.locationName) | Just specialLocation <- specialLocations]

toSpecialLocationNames :: [(Text, Text)] -> Maybe [Text] -> Maybe [Text]
toSpecialLocationNames specialLocationNames =
  fmap (map resolveName)
  where
    resolveName locationId = maybe locationId snd (find ((== locationId) . fst) specialLocationNames)

-- | For Coins: display amount from CoinsConfig. For Cash/Coupons: stored rewardValue.
resolveDisplayRewardValue :: DIJM.IncentiveJourneyMilestone -> Flow (Maybe Int)
resolveDisplayRewardValue milestone =
  case milestone.rewardType of
    DIJM.Coins ->
      case milestone.rewardConfigId of
        Nothing -> pure milestone.rewardValue
        Just configId -> do
          mbConfig <- SQCC.findById configId
          pure $ maybe milestone.rewardValue (Just . (.coins)) mbConfig
    DIJM.Cash -> pure milestone.rewardValue
    DIJM.Coupons -> pure milestone.rewardValue

getIncentiveJourneyList ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow API.IncentiveJourneyListRes
getIncentiveJourneyList (mbPersonId, merchantId, merchantOpCityId) mbActive mbDate _mbLimit _mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  transporterConfig <-
    getOneConfig
      (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehicle <- QVeh.findById driverId >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
  let vehCategory = VecVariant.castVehicleVariantToVehicleCategory vehicle.variant
      mbVehicleVariant = Just vehicle.variant
  localTime <- case mbDate >>= parseDateText of
    Just day -> pure $ UTCTime day 0
    Nothing -> getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let journeyTags = SLJourney.parseJourneyTags driver.driverTag
      onlyEnabled = fromMaybe True mbActive
  if null journeyTags
    then pure API.IncentiveJourneyListRes {journeys = []}
    else do
      enabledJourneys <-
        if onlyEnabled
          then
            getConfig
              ( IncentiveJourneyDimensions
                  { merchantOperatingCityId = merchantOpCityId.getId,
                    journeyId = Nothing,
                    enabled = Just True,
                    vehicleCategory = Nothing,
                    vehicleVariant = Nothing
                  }
              )
              (Just $ CQJourney.findEnabledByMerchantOperatingCityId merchantOpCityId)
          else
            getConfig
              ( IncentiveJourneyDimensions
                  { merchantOperatingCityId = merchantOpCityId.getId,
                    journeyId = Nothing,
                    enabled = Nothing,
                    vehicleCategory = Nothing,
                    vehicleVariant = Nothing
                  }
              )
              (Just $ CQJourney.findByMerchantOperatingCityId merchantOpCityId)
      let matching =
            filter
              ( \j ->
                  j.merchantId == merchantId
                    && j.driverTag `elem` journeyTags
                    && SLJourney.matchesJourneyVehicle j vehCategory mbVehicleVariant
              )
              enabledJourneys
      -- Prefer currently-active journey; else first match for "come back later". Return at most 1.
      case SLJourney.selectPreferredJourney localTime matching of
        Nothing -> pure API.IncentiveJourneyListRes {journeys = []}
        Just journey -> do
          milestones <- SLJourney.loadJourneyMilestones merchantOpCityId journey.id
          let periodKey = SLJourney.mkJourneyPeriodKey localTime journey
          statsRows <- CQStats.findByDriverIdAndJourneyIdAndPeriodKey driverId journey.id periodKey
          specialLocationNames <- buildSpecialLocationNames milestones
          items <- mapM (toMilestoneItem specialLocationNames statsRows) milestones
          pure $
            API.IncentiveJourneyListRes
              { journeys =
                  [ API.IncentiveJourneyListItem
                      { journeyId = journey.id,
                        name = journey.name,
                        description = journey.description,
                        journeyType = journey.journeyType <|> Just DIJ.Daily,
                        timeBounds = journey.timeBounds,
                        startDate = journey.startDate,
                        endDate = journey.endDate,
                        vehicleCategory = journey.vehicleCategory,
                        vehicleVariant = journey.vehicleVariant,
                        enabled = journey.enabled,
                        milestones = items
                      }
                  ]
              }

toMilestoneItem :: [(Text, Text)] -> [DIJS.IncentiveJourneyStats] -> DIJM.IncentiveJourneyMilestone -> Flow API.IncentiveJourneyMilestoneItem
toMilestoneItem specialLocationNames statsRows milestone = do
  let mbStats = find (\s -> s.milestoneId == milestone.id) statsRows
  displayRewardValue <- resolveDisplayRewardValue milestone
  pure $
    API.IncentiveJourneyMilestoneItem
      { milestoneId = milestone.id,
        description = milestone.description,
        order = milestone.order,
        conditionType = milestone.conditionType,
        conditionOperator = conditionOperatorOrDefault milestone.conditionOperator,
        conditionValue = milestone.conditionValue,
        pickupSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.pickupSpecialLocationIds,
        dropSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.dropSpecialLocationIds,
        rewardType = milestone.rewardType,
        rewardValue = displayRewardValue,
        status = maybe DIJS.NotStarted (.status) mbStats,
        currentValue = maybe 0 (.currentValue) mbStats
      }

getIncentiveJourneyHistory ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow API.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory (mbPersonId, _merchantId, merchantOpCityId) mbDate mbLimit mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  transporterConfig <-
    getOneConfig
      (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let historyDay = fromMaybe (utctDay localTime) (mbDate >>= parseDateText)
      (dayStart, dayEnd) = QStats.mkLocalDayUtcBounds historyDay transporterConfig.timeDiffFromUtc
      weeklyPeriodKey = SLJourney.mkWeeklyPeriodKey (UTCTime historyDay 0)
      limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dailyStatsRows <- QStats.findHistoryByDriverIdAndCreatedAtRange driverId dayStart dayEnd Nothing Nothing
  weeklyStatsRows <- QStats.findByDriverIdAndPeriodKey driverId weeklyPeriodKey
  let dailyStatsIds = map (.id) dailyStatsRows
      statsRows = dailyStatsRows <> filter (\stats -> stats.id `notElem` dailyStatsIds) weeklyStatsRows
  let journeyIds = nub $ map (.journeyId) statsRows
  expanded <-
    concat
      <$> forM
        journeyIds
        ( \journeyId -> do
            mbJourney <-
              getOneConfig
                ( IncentiveJourneyDimensions
                    { merchantOperatingCityId = merchantOpCityId.getId,
                      journeyId = Just journeyId,
                      enabled = Nothing,
                      vehicleCategory = Nothing,
                      vehicleVariant = Nothing
                    }
                )
                (Just $ CQJourney.findById journeyId >>= maybe (pure []) (pure . (: [])))
            milestones <- SLJourney.loadJourneyMilestones merchantOpCityId journeyId
            specialLocationNames <- buildSpecialLocationNames milestones
            let journeyName = maybe journeyId.getId (.name) mbJourney
                journeyType = (mbJourney >>= (.journeyType)) <|> Just DIJ.Daily
                journeyStats = filter (\s -> s.journeyId == journeyId) statsRows
                defaultPeriodKey = maybe ("Day:" <> T.pack (show historyDay)) (.periodKey) (listToMaybe journeyStats)
            forM milestones $ \milestone -> do
              let mbStats = find (\s -> s.milestoneId == milestone.id) journeyStats
              displayRewardValue <- resolveDisplayRewardValue milestone
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
          milestoneDescription = milestone.description,
          milestoneOrder = milestone.order,
          conditionType = stats.conditionType,
          conditionOperator = conditionOperatorOrDefault stats.conditionOperator,
          conditionValue = stats.conditionValue,
          pickupSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.pickupSpecialLocationIds,
          dropSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.dropSpecialLocationIds,
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
          milestoneDescription = milestone.description,
          milestoneOrder = milestone.order,
          conditionType = milestone.conditionType,
          conditionOperator = conditionOperatorOrDefault milestone.conditionOperator,
          conditionValue = milestone.conditionValue,
          pickupSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.pickupSpecialLocationIds,
          dropSpecialLocationNames = toSpecialLocationNames specialLocationNames milestone.dropSpecialLocationIds,
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
    DIJS.InProgress -> Nothing
    DIJS.NotStarted -> Nothing

parseDateText :: Text -> Maybe Day
parseDateText t = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
