module Lib.Yudhishthira.Event.KaalChakra.Jobs
  ( runKaalChakraAndRescheduleJob,
    runKaalChakraUpdateTagsJob,
  )
where

import Kernel.Prelude
import qualified Kernel.Types.SlidingWindowCounters as SWCT
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.Scheduler
import qualified Lib.Yudhishthira.Event.KaalChakra.Internal as Event
import qualified Lib.Yudhishthira.Types as LYT

mkRunKaalChakraJobReq ::
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  UTCTime ->
  LYT.RunKaalChakraJobReq
mkRunKaalChakraJobReq chakra LYT.KaalChakraJobData {..} now = do
  let action = LYT.SCHEDULE now -- error handle is different for RUN and SCHEDULE cases
  let completeOldJob = Nothing -- no matter for job handler
  LYT.RunKaalChakraJobReq {usersSet = LYT.ALL_USERS, ..}

mkRunKaalChakraUpdateTagJobReq ::
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  LYT.UpdateKaalBasedTagsJobReq
mkRunKaalChakraUpdateTagJobReq chakra LYT.UpdateKaalBasedTagsData {..} = LYT.UpdateKaalBasedTagsJobReq {usersSet = LYT.ALL_USERS, ..}

runKaalChakraAndRescheduleJob ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  m ExecutionResult
runKaalChakraAndRescheduleJob h chakra jobData = do
  logInfo $ "Running " <> show chakra <> " Job"
  now <- getCurrentTime
  let req = mkRunKaalChakraJobReq chakra jobData now
  eventResult <- Event.kaalChakraEvent req
  timeAfterRun <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) timeAfterRun
    LYT.Completed -> do
      void $ Event.clearEventData chakra Nothing --- passing Nothing will only clear the batch number key, which is required for the next job from 0 again
      whenJust eventResult.eventId $ \eventId -> do
        let updateUserTagJobData = LYT.mkUpdateTagDataFromKaalChakraJobData req eventId
        h.createUpdateUserTagDataJob chakra updateUserTagJobData (addUTCTime 60 timeAfterRun) -- starting updateTags job after 60 seconds of caching UserData
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime timeAfterRun chakra
      h.createFetchUserDataJob chakra jobData newScheduleTime
      pure $ Terminate "Fetch user data job failed"

runKaalChakraUpdateTagsJob ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  m ExecutionResult
runKaalChakraUpdateTagsJob h chakra jobData = do
  logInfo $ "Running " <> show chakra <> "UpdateTag Job"
  let req = mkRunKaalChakraUpdateTagJobReq chakra jobData
  eventResult <- Event.updateUserTagsHandler h req
  now <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) now
    LYT.Completed -> do
      void $ Event.clearEventData req.chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime now req.chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      h.createFetchUserDataJob chakra fetchUserDataJobData newScheduleTime
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime now chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      h.createFetchUserDataJob chakra fetchUserDataJobData newScheduleTime
      pure $ Terminate "Update user tags job failed"

-- for now keeping the rescheduling of ckahra job to be at night 12:00 + some delta based on chakra type
getNextChakraTime :: UTCTime -> LYT.Chakra -> UTCTime
getNextChakraTime now chakra = do
  let endOfDayToday = SWC.incrementPeriod SWCT.Days now
  -- above function gives us the end of day time for the next day,
  -- i.e. 00:00 of the next day no matter what the time today you run this function.

  -- adding n - 1 days below due to above.
  flip addUTCTime endOfDayToday $ case chakra of
    LYT.Daily -> 86400 + 3600 - 19800
    LYT.Weekly -> 7 * 86400 + 7200 - 19800
    LYT.Monthly -> 30 * 86400 + 10800 - 19800
    LYT.Quarterly -> 90 * 86400 + 14400 - 19800
