module Domain.Action.Internal.ProcessingChangeOnline (processingChangeOnline, updateOnlineDurationDuringFetchingDailyStats) where

import Data.Time (DiffTime, UTCTime (..), addUTCTime, diffUTCTime)
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude hiding (id, state)
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (Seconds (..), generateGUIDText, getCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Logging (logDebug, logError, logInfo)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DailyStats as QDS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import Tools.Error

processingChangeOnline ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  Maybe DriverInfo.DriverMode ->
  m ()
processingChangeOnline driverId transporterConfig mode = do
  withOnlineDurationLock driverId transporterConfig $ \driverInfo now onlineDurationCalculateFrom -> do
    let previousMode = driverInfo.mode
    when (previousMode == Just DriverInfo.ONLINE && mode /= Just DriverInfo.ONLINE) $ do
      updateOnlineDuration driverId transporterConfig driverInfo now onlineDurationCalculateFrom
    when (mode == Just DriverInfo.ONLINE && previousMode /= Just DriverInfo.ONLINE) $
      QDI.updateOnlineDurationRefreshedAt driverId now

updateOnlineDurationDuringFetchingDailyStats ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  m ()
updateOnlineDurationDuringFetchingDailyStats driverId transporterConfig = do
  withOnlineDurationLock driverId transporterConfig $ \driverInfo now onlineDurationCalculateFrom -> do
    when (driverInfo.mode == Just DriverInfo.ONLINE) $ do
      updateOnlineDuration driverId transporterConfig driverInfo now onlineDurationCalculateFrom

withOnlineDurationLock ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  (DriverInfo.DriverInformation -> UTCTime -> UTCTime -> m ()) ->
  m ()
withOnlineDurationLock driverId transporterConfig action = do
  whenJust transporterConfig.onlineDurationCalculateFrom $ \onlineDurationCalculateFrom -> do
    now <- getCurrentTime
    when (onlineDurationCalculateFrom <= now) $ do
      -- To avoid race condition we need to fetch driveInfo, dailyStats, driverStats inside of lock
      Redis.whenWithLockRedis (updateDriverOnlineDurationLockKey driverId) 60 $ do
        driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
        action driverInfo now onlineDurationCalculateFrom

updateOnlineDuration ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  DriverInfo.DriverInformation ->
  UTCTime ->
  UTCTime ->
  m ()
updateOnlineDuration driverId transporterConfig driverInfo now onlineDurationCalculateFrom = do
  let timeDiffFromUtc = transporterConfig.timeDiffFromUtc
      localTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
      merchantLocalDate = utctDay localTime
      mbMaxOnlineDurationDays = transporterConfig.maxOnlineDurationDays
      mbLastOnlineFrom = max onlineDurationCalculateFrom <$> driverInfo.onlineDurationRefreshedAt
      mbLastOnlineFromLocal = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) <$> mbLastOnlineFrom
      numDaysAgo = fromMaybe 10 mbMaxOnlineDurationDays
      limitNumDaysAgoLocal = addUTCTime (secondsToNominalDiffTime . Seconds $ (- numDaysAgo) * 86400) localTime
      lastOnlineFromLocalLimited = maybe localTime (max limitNumDaysAgoLocal) mbLastOnlineFromLocal
      startDayTimeLocal = UTCTime (utctDay localTime) 0
      twoDaysInSeconds = Seconds 172800
  mbDailyStats <- QDS.findByDriverIdAndDate driverId merchantLocalDate
  let newOnlineDurationDaily = calcOnlineDuration localTime mbDailyStats lastOnlineFromLocalLimited
      newOnlineDuration = diffUTCTimeInSeconds localTime $ fromMaybe localTime mbLastOnlineFromLocal
  when (newOnlineDuration > twoDaysInSeconds) $
    logInfo $ "Online duration more than 2 days. DriverId: " <> driverId.getId <> "; duration: " <> show newOnlineDuration
  when (lastOnlineFromLocalLimited == limitNumDaysAgoLocal) . logError $
    "The limit of " <> show numDaysAgo <> " days has been reached during the calculation of onlineDuration. DriverId: " <> driverId.getId <> "; duration: " <> show newOnlineDuration
  whenNothing_ driverInfo.onlineDurationRefreshedAt . logDebug $ "OnlineDurationRefreshedAt is Nothing. DriverId: " <> driverId.getId
  addDataToDailyStats mbDailyStats merchantLocalDate newOnlineDurationDaily
  QDI.updateOnlineDurationRefreshedAt driverId now
  when (lastOnlineFromLocalLimited < startDayTimeLocal) $ setOnlineDurationInDailyStatsForPrevDays merchantLocalDate lastOnlineFromLocalLimited
  addDataToDriverStats newOnlineDuration
  where
    setOnlineDurationInDailyStatsForPrevDays todayMerchantLocalDate lastOnlineFrom = do
      let lastOnlineFromMerchantLocalDate = utctDay lastOnlineFrom
      mbPrevDayDailyStats <- QDS.findByDriverIdAndDate driverId lastOnlineFromMerchantLocalDate
      let succLastOnlineFromMerchantLocalDate = succ lastOnlineFromMerchantLocalDate
          succStartDayTime = UTCTime succLastOnlineFromMerchantLocalDate 0
          newPrevDayOnlineDuration = calcPreviousDayOnlineDuration succStartDayTime lastOnlineFrom mbPrevDayDailyStats
      addDataToDailyStats mbPrevDayDailyStats lastOnlineFromMerchantLocalDate newPrevDayOnlineDuration
      when (todayMerchantLocalDate > succLastOnlineFromMerchantLocalDate) $
        setOnlineDurationInDailyStatsForPrevDays todayMerchantLocalDate succStartDayTime

    addDataToDailyStats mbDailyStats' =
      if isJust mbDailyStats'
        then QDS.updateOnlineDurationByDriverId driverId
        else createNewDailyStats

    createNewDailyStats merchantLocDate onlineDuration = do
      let merchantId = transporterConfig.merchantId
          merchantOpCityId = transporterConfig.merchantOperatingCityId
      id <- generateGUIDText
      merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      QDS.create $
        DDS.DailyStats
          { id,
            driverId,
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

    addDataToDriverStats newOnlineDuration = do
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
   in onlineDuration + diffUTCTimeInSeconds lastOnlineTo lastOnlineFrom'

calcPreviousDayOnlineDuration ::
  UTCTime ->
  UTCTime ->
  Maybe DDS.DailyStats ->
  Seconds
calcPreviousDayOnlineDuration succStartDayTime lastOnlineFrom mbPrevDayDailyStats =
  let prevDayOnlineDuration = fromMaybe (Seconds 0) $ mbPrevDayDailyStats >>= (.onlineDuration)
   in prevDayOnlineDuration + diffUTCTimeInSeconds succStartDayTime lastOnlineFrom

-- To avoid rounding error accumulation, each value was rounded to whole seconds
diffUTCTimeInSeconds :: UTCTime -> UTCTime -> Seconds
diffUTCTimeInSeconds to from = Seconds (round $ diffUTCTime (roundUTCTimeToSecond to) (roundUTCTimeToSecond from))

roundUTCTimeToSecond :: UTCTime -> UTCTime
roundUTCTimeToSecond (UTCTime day dt) = UTCTime day (fromIntegral $ floor @DiffTime @Integer dt)
