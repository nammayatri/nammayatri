module SharedLogic.KaalChakra.Actions (kaalChakraAction, Action) where

import Data.Time hiding (getCurrentTime)
import Domain.Action.UI.LmsModule
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Yudhishthira.Storage.Queries.TagActionNotificationConfig as QTAN
import qualified Lib.Yudhishthira.Types.TagActionNotificationConfig as DRN
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()

data Action
  = SAFE_TO_UNSAFE_COHORT
  | UNSAFE_TO_SAFE_COHORT
  deriving (Show, Read)

kaalChakraAction ::
  ( EsqDBFlow m r,
    SchedulerFlow r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Id DPerson.Person ->
  Maybe Action ->
  Text ->
  m ()
kaalChakraAction mbMerchantOperatingCityId driverId mbAction notificationKey = do
  whenJust mbMerchantOperatingCityId $ \cityId -> do
    notificationConfigs <- QTAN.findAllByMerchantOperatingCityIdAndNotificationKey (cast cityId) notificationKey
    void $ scheduleTagActionNotificationJob driverId `mapM` notificationConfigs
  whenJust mbAction $ \action -> do
    case action of
      SAFE_TO_UNSAFE_COHORT -> do
        logInfo $ "Kaal chakra action: " <> show action <> "; driverId: " <> show driverId
        updateExpiryTimeForDowngradedTag driverId
      UNSAFE_TO_SAFE_COHORT -> do
        logInfo $ "Kaal chakra action: " <> show action <> "; driverId: " <> show driverId

scheduleTagActionNotificationJob ::
  ( MonadFlow m,
    SchedulerFlow r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DPerson.Person ->
  DRN.TagActionNotificationConfig ->
  m ()
scheduleTagActionNotificationJob driverId DRN.TagActionNotificationConfig {..} = do
  now <- getCurrentTime
  let dfCalculationJobTs = max 2 (diffUTCTime (nextOccurrenceUTC notifyAt now) now) -- Buffer of 2 seconds in case of <=0 timeDiff
  createJobIn @_ @'ScheduleTagActionNotification (Just (cast merchantId)) (Just (cast merchantOperatingCityId)) dfCalculationJobTs $
    ScheduleTagActionNotificationJobData
      { merchantId = cast merchantId,
        merchantOperatingCityId = cast merchantOperatingCityId,
        notificationType = castNotificationType notificationType,
        notificationKey,
        driverId
      }
  where
    castNotificationType :: DRN.NotificationType -> DTRN.NotificationType
    castNotificationType DRN.SMS = DTRN.SMS
    castNotificationType DRN.PN = DTRN.PN
    castNotificationType DRN.WHATSAPP = DTRN.WHATSAPP
    castNotificationType DRN.OVERLAY = DTRN.OVERLAY
    castNotificationType DRN.CALL = DTRN.CALL

    nextOccurrenceUTC :: TimeOfDay -> UTCTime -> UTCTime
    nextOccurrenceUTC targetTime now = do
      let targetUTCTimeToday = UTCTime (utctDay now) (timeOfDayToTime targetTime)
      -- If the target time today has already passed, return the same time tomorrow
      if targetUTCTimeToday > now
        then targetUTCTimeToday
        else UTCTime (addDays 1 $ utctDay now) (timeOfDayToTime targetTime)
