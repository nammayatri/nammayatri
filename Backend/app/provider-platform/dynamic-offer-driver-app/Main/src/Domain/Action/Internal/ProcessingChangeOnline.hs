module Domain.Action.Internal.ProcessingChangeOnline (processingChangeOnline, updateOnlineDurationDuringFetchingDailyStats) where

import Data.Time (Day (..), UTCTime (..), addUTCTime, diffUTCTime)
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude hiding (id, state)
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (Seconds (..), generateGUIDText, getCurrentTime, getLocalCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Logging (logDebug, logError, logInfo)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.DailyStatsExtra as SQDSE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIE
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import Tools.Error

processingChangeOnline ::
  (CacheFlow m r, EsqDBFlow m r) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Seconds ->
  Maybe Int ->
  Maybe DriverInfo.DriverMode ->
  m ()
processingChangeOnline (driverId, merchantId, merchantOpCityId) timeDiffFromUtc mbMaxOnlineDurationDays mode =
  -- To avoid race condition we need to fetch driveInfo, dailyStats, driverStats inside of lock
  Redis.whenWithLockRedis (updateDriverOnlineDurationLockKey driverId) 60 $ do
    driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    localTime <- getLocalCurrentTime timeDiffFromUtc
    let previousMode = driverInfo.mode
        merchantLocalDate = utctDay localTime
    now <- getCurrentTime
    mbDailyStats <- SQDS.findByDriverIdAndDate driverId merchantLocalDate
    when (previousMode == Just DriverInfo.ONLINE && mode /= Just DriverInfo.ONLINE) $ do
      updateOnlineDuration (driverId, merchantId, merchantOpCityId) timeDiffFromUtc driverInfo now localTime merchantLocalDate mbMaxOnlineDurationDays mbDailyStats
    when (mode == Just DriverInfo.ONLINE && previousMode /= Just DriverInfo.ONLINE) $
      QDIE.updateOnlineDurationRefreshedAt driverId now

updateOnlineDuration ::
  (CacheFlow m r, EsqDBFlow m r) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Seconds ->
  DriverInfo.DriverInformation ->
  UTCTime ->
  UTCTime ->
  Day ->
  Maybe Int ->
  Maybe DDS.DailyStats ->
  m ()
updateOnlineDuration (driverId, merchantId, merchantOpCityId) timeDiffFromUtc driverInfo now localTime merchantLocalDate mbMaxOnlineDurationDays mbDailyStats = do
  let mbLastOnlineFrom = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) <$> driverInfo.onlineDurationRefreshedAt
      numDaysAgo = fromMaybe 10 mbMaxOnlineDurationDays
      limitLastOnlineFrom = addUTCTime (secondsToNominalDiffTime . Seconds $ - numDaysAgo * 86400) localTime
      lastOnlineFrom = maybe localTime (max limitLastOnlineFrom) mbLastOnlineFrom
      newOnlineDuration = calcOnlineDuration localTime mbDailyStats lastOnlineFrom
      startDayTime = UTCTime (utctDay localTime) 0
      twoDaysInSeconds = Seconds 172800
  when (newOnlineDuration > twoDaysInSeconds) . logInfo $
    "Online duration more than 2 days. DriverId: " <> driverId.getId
  when (lastOnlineFrom == limitLastOnlineFrom) . logError $
    "The limit of " <> show numDaysAgo <> " days has been reached during the calculation of onlineDuration. DriverId: " <> driverId.getId
  whenNothing_ mbLastOnlineFrom . logDebug $ "OnlineDurationRefreshedAt is Nothing. DriverId: " <> driverId.getId
  addDataToDailyStats mbDailyStats merchantLocalDate newOnlineDuration
  QDIE.updateOnlineDurationRefreshedAt driverId now
  when (lastOnlineFrom < startDayTime) $ setOnlineDurationInDailyStatsForPrevDays merchantLocalDate lastOnlineFrom
  addDataToDriverStats $ fromMaybe localTime mbLastOnlineFrom
  where
    setOnlineDurationInDailyStatsForPrevDays todayMerchantLocalDate lastOnlineFrom = do
      let lastOnlineFromMerchantLocalDate = utctDay lastOnlineFrom
      mbPrevDayDailyStats <- SQDS.findByDriverIdAndDate driverId lastOnlineFromMerchantLocalDate
      let dayWithoutOneSecond = 86399 --  == 24*60*60 - 1
          endDayTime = UTCTime lastOnlineFromMerchantLocalDate dayWithoutOneSecond
          newPrevDayOnlineDuration = calcPreviousDayOnlineDuration endDayTime lastOnlineFrom mbPrevDayDailyStats
          succLastOnlineFromMerchantLocalDate = succ lastOnlineFromMerchantLocalDate
      addDataToDailyStats mbPrevDayDailyStats lastOnlineFromMerchantLocalDate newPrevDayOnlineDuration
      when (todayMerchantLocalDate > succLastOnlineFromMerchantLocalDate)
        . setOnlineDurationInDailyStatsForPrevDays todayMerchantLocalDate
        $ UTCTime succLastOnlineFromMerchantLocalDate 0

    addDataToDailyStats mbDailyStats' =
      if isJust mbDailyStats'
        then SQDSE.updateOnlineDurationByDriverId driverId
        else createNewDailyStats

    createNewDailyStats merchantLocDate onlineDuration = do
      id <- generateGUIDText
      merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      SQDS.create $
        DDS.DailyStats
          { id = id,
            driverId = driverId,
            totalEarnings = 0.0,
            numRides = 0,
            totalDistance = 0,
            tollCharges = 0.0,
            bonusEarnings = 0.0,
            merchantLocalDate = merchantLocDate,
            currency = merchantOpCity.currency,
            distanceUnit = merchantOpCity.distanceUnit,
            activatedValidRides = 0,
            referralEarnings = 0,
            referralCounts = 0,
            payoutStatus = DDS.Initialized,
            payoutOrderId = Nothing,
            payoutOrderStatus = Nothing,
            createdAt = now,
            updatedAt = now,
            cancellationCharges = 0.0,
            tipAmount = 0.0,
            totalRideTime = 0,
            numDriversOnboarded = 0,
            numFleetsOnboarded = 0,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            onlineDuration = Just onlineDuration
          }

    addDataToDriverStats lastOnlineFrom = do
      let newOnlineDuration = Seconds (floor $ diffUTCTime localTime lastOnlineFrom)
      QDriverStats.findById driverId >>= \case
        Just driverStats -> do
          let totalOnlineDuration = driverStats.onlineDuration + newOnlineDuration
          QDriverStats.updateOnlineDuration totalOnlineDuration driverId
        Nothing -> logError $ "DriverStats does not found: driverId: " <> driverId.getId

updateDriverOnlineDurationLockKey :: Id DP.Person -> Text
updateDriverOnlineDurationLockKey id = "DriveOnlineDuration:PersonId-" <> id.getId

calcOnlineDuration ::
  UTCTime ->
  Maybe DDS.DailyStats ->
  UTCTime ->
  Seconds
calcOnlineDuration localTime mbDailyStats lastOnlineFrom =
  let lastOnlineTo = localTime
      startDayTime = UTCTime (utctDay localTime) 0
      lastOnlineFrom' = max startDayTime lastOnlineFrom
      mbLastOnlineDuration = mbDailyStats >>= (.onlineDuration)
      onlineDuration = if lastOnlineFrom < startDayTime then Seconds 0 else fromMaybe (Seconds 0) mbLastOnlineDuration
   in onlineDuration + Seconds (floor $ diffUTCTime lastOnlineTo lastOnlineFrom')

calcPreviousDayOnlineDuration ::
  UTCTime ->
  UTCTime ->
  Maybe DDS.DailyStats ->
  Seconds
calcPreviousDayOnlineDuration endDayTime lastOnlineFrom mbPrevDayDailyStats =
  let prevDayOnlineDuration = fromMaybe (Seconds 0) $ mbPrevDayDailyStats >>= (.onlineDuration)
   in prevDayOnlineDuration + Seconds (floor $ diffUTCTime endDayTime lastOnlineFrom)

updateOnlineDurationDuringFetchingDailyStats ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  m ()
updateOnlineDurationDuringFetchingDailyStats driverId transporterConfig = do
  driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  -- To avoid race condition we need to fetch driveInfo, dailyStats, driverStats inside of lock
  Redis.whenWithLockRedis (updateDriverOnlineDurationLockKey driverId) 60 $ do
    driverInfo <- QDIE.findByIdAndVerified driverId Nothing >>= fromMaybeM DriverInfoNotFound
    now <- getCurrentTime
    let localTime = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now
        merchantLocalDate = utctDay localTime
    mbDailyStats <- SQDS.findByDriverIdAndDate driver.id merchantLocalDate
    updateOnlineDuration (driverId, driver.merchantId, driver.merchantOperatingCityId) transporterConfig.timeDiffFromUtc driverInfo now localTime merchantLocalDate transporterConfig.maxOnlineDurationDays mbDailyStats
