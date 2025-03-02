module SharedLogic.KaalChakra.Actions (kaalChakraAction, Action) where

import qualified Domain.Types.Person as DPerson
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

data Action
  = SAFE_TO_UNSAFE_COHORT
  | UNSAFE_TO_SAFE_COHORT
  deriving (Show, Read)

kaalChakraAction ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Id DPerson.Person ->
  Maybe Action ->
  Text ->
  m ()
kaalChakraAction personId mbAction _ = do
  whenJust mbAction $ \action -> do
    case action of
      SAFE_TO_UNSAFE_COHORT -> do
        logInfo $ "Kaal chakra action: " <> show action <> "; personId: " <> show personId
      UNSAFE_TO_SAFE_COHORT -> do
        logInfo $ "Kaal chakra action: " <> show action <> "; personId: " <> show personId

-- scheduleTagActionNotificationJob ::
--   ( MonadFlow m,
--     SchedulerFlow r,
--     EsqDBFlow m r,
--     CacheFlow m r
--   ) =>
--   Id DPerson.Person ->
--   DRN.TagActionNotificationConfig ->
--   m ()
-- scheduleTagActionNotificationJob driverId DRN.TagActionNotificationConfig {..} = do
--   now <- getCurrentTime
--   let dfCalculationJobTs = max 2 (diffUTCTime notifyAt now) -- Buffer of 2 seconds in case of <=0 timeDiff
--   createJobIn @_ @'ScheduledRideNotificationsToDriver (Just merchantId) (Just merchantOperatingCityId) dfCalculationJobTs $
--     ScheduleTagActionNotificationJobData
--       { merchantId,
--         merchantOperatingCityId,
--         notificationType,
--         notificationKey,
--         driverId
--       }
--   where
--     nextOccurrenceUTC :: TimeOfDay -> UTCTime -> m UTCTime
--     nextOccurrenceUTC targetTime now = do
--       let targetUTCTimeToday = UTCTime (utctDay now) (timeOfDayToTime targetTime)
--       -- If the target time today has already passed, return the same time tomorrow
--       return $
--         if targetUTCTimeToday > now
--           then targetUTCTimeToday
--           else UTCTime (addDays 1 $ utctDay now) (timeOfDayToTime targetTime)
