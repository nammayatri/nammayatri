module Domain.Action.Internal.ProcessingChangeOnline where

import Data.Time (UTCTime (..), addUTCTime, diffUTCTime)
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id, state)
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Error (MerchantError (MerchantOperatingCityNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common (Seconds (..), generateGUIDText, getCurrentTime, getLocalCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Logging (logDebug, logError, logInfo)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.DailyStatsExtra as SQDSE
import qualified Storage.Queries.DriverInformationExtra as QDIE

processingChangeOnline ::
  (CacheFlow m r, EsqDBFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Seconds ->
  Maybe Int ->
  DriverInfo.DriverInformation ->
  Maybe DriverInfo.DriverMode ->
  m ()
processingChangeOnline (driverId, merchantId, merchantOpCityId) timeDiffFromUtc mbMaxOnlineDurationDays driverInfo mode =
  Redis.whenWithLockRedis (updateDriverOnlineDurationLockKey driverId) 60 $ do
    localTime <- getLocalCurrentTime timeDiffFromUtc
    let previousMode = driverInfo.mode
        merchantLocalDate = utctDay localTime
    now <- getCurrentTime
    mbDailyStats <- SQDS.findByDriverIdAndDate driverId merchantLocalDate
    when (previousMode == Just DriverInfo.ONLINE && mode /= Just DriverInfo.ONLINE) $ do
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
    when (mode == Just DriverInfo.ONLINE && previousMode /= Just DriverInfo.ONLINE) $
      QDIE.updateOnlineDurationRefreshedAt driverId now
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

    addDataToDailyStats mbDailyStats =
      if isJust mbDailyStats
        then SQDSE.updateOnlineDurationByDriverId driverId
        else createNewDailyStats

    createNewDailyStats merchantLocalDate onlineDuration = do
      id <- generateGUIDText
      now <- getCurrentTime
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
            merchantLocalDate = merchantLocalDate,
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

updateDriverOnlineDurationLockKey :: Id SP.Person -> Text
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
