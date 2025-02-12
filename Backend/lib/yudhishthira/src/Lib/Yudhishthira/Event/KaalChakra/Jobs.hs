module Lib.Yudhishthira.Event.KaalChakra.Jobs
  ( runKaalChakraAndRescheduleJob,
    runKaalChakraUpdateTagsJob,
    getNextChakraTime,
  )
where

import qualified Data.Time.Clock as Time
import Kernel.Prelude
import Kernel.Utils.Common
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
        let updateUserTagJobData = LYT.mkUpdateTagDataFromKaalChakraJobData req eventId jobData.startTime
        h.createUpdateUserTagDataJob chakra updateUserTagJobData (addUTCTime 60 timeAfterRun) -- starting updateTags job after 60 seconds of caching UserData
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime jobData.startTime timeAfterRun chakra
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
  timeAfterRun <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) timeAfterRun
    LYT.Completed -> do
      void $ Event.clearEventData req.chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime jobData.startTime timeAfterRun req.chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      h.createFetchUserDataJob chakra fetchUserDataJobData newScheduleTime
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime jobData.startTime timeAfterRun chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      h.createFetchUserDataJob chakra fetchUserDataJobData newScheduleTime
      pure $ Terminate "Update user tags job failed"

getNextChakraTime :: Maybe UTCTime -> UTCTime -> LYT.Chakra -> UTCTime
getNextChakraTime mbStartTime finishTime chakra = do
  let (chakraDiffHours, chakraDays) :: (Integer, Integer) = case chakra of
        LYT.Daily -> (1, 1)
        LYT.Weekly -> (2, 7)
        LYT.Monthly -> (3, 30)
        LYT.Quarterly -> (4, 90)
      timeZoneDiffTime = 19800 -- 5.5*3600
      chakraDiffTime :: Time.DiffTime = Time.secondsToDiffTime $ 86400 + chakraDiffHours * 3600 - timeZoneDiffTime
      startTime = fromMaybe (previousDiffTime chakraDiffTime finishTime) mbStartTime
      nextChakraTime = addDays chakraDays startTime
  max nextChakraTime (nextDiffTime (Time.utctDayTime nextChakraTime) finishTime) -- in case of very long jobs

addDays :: Integer -> UTCTime -> UTCTime
addDays n = addUTCTime $ fromInteger (n * 86400)

nextDiffTime :: Time.DiffTime -> UTCTime -> UTCTime
nextDiffTime diff time = do
  let currentDiffTime = time {Time.utctDayTime = diff}
  if currentDiffTime > time then currentDiffTime else addDays 1 currentDiffTime

previousDiffTime :: Time.DiffTime -> UTCTime -> UTCTime
previousDiffTime diff time = do
  let currentDiffTime = time {Time.utctDayTime = diff}
  if currentDiffTime < time then currentDiffTime else addDays (-1) currentDiffTime
