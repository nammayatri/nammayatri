module Lib.Yudhishthira.Event.KaalChakra.Jobs
  ( runKaalChakraAndRescheduleJob,
    runKaalChakraUpdateTagsJob,
    recordChakraJobCancelled,
    getNextChakraTime,
  )
where

import qualified Data.Time.Clock as Time
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Yudhishthira.Event.KaalChakra.Internal as Event
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.Queries.KaalChakraJobRun as QKCJR
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun as DKCJR

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

-- Best-effort run-history write for the Control Center Jobs tab. Never lets a
-- bug here fail the actual chakra job: any exception is logged and swallowed.
-- One row per (eventId, phase); opened on the first batch and updated after every
-- later batch. A failed batch is retried with the same eventId, so the row moving
-- on from Failed counts as one retry, and the failure fields always describe the
-- latest attempt (a run that recovers ends Completed with no stale error).
recordChakraJobRun ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  Id AnyJob ->
  UTCTime ->
  LYT.Chakra ->
  DKCJR.KaalChakraJobRunPhase ->
  LYT.RunKaalChakraJobRes ->
  m ()
recordChakraJobRun h jobId batchStartedAt chakra phase eventResult =
  handle (\(err :: SomeException) -> logError $ "Failed to record kaal chakra job run history: " <> show err) $
    whenJust eventResult.eventId $ \eventId ->
      case (h.merchantId, h.merchantOperatingCityId) of
        (Just merchantId, Just merchantOperatingCityId) -> do
          now <- getCurrentTime
          let (status, mbEndedAt) = case eventResult.chakraBatchState of
                LYT.Continue _ -> (DKCJR.Running, Nothing)
                LYT.Completed -> (DKCJR.Completed, Just now)
                LYT.Failed -> (DKCJR.Failed, Just now)
              batchSucceeded = status /= DKCJR.Failed
              batchUsers = fromMaybe 0 eventResult.batchUsersCount
          existingRows <- QKCJR.findAllByEventId eventId
          case find ((== phase) . (.phase)) existingRows of
            Nothing -> do
              newId <- generateGUID
              QKCJR.create
                DKCJR.KaalChakraJobRun
                  { id = newId,
                    eventId,
                    chakra,
                    phase,
                    schedulerJobId = Just jobId.getId,
                    merchantId,
                    merchantOperatingCityId,
                    status,
                    startedAt = batchStartedAt,
                    endedAt = mbEndedAt,
                    usersProcessed = Just batchUsers,
                    batchesProcessed = Just (if batchSucceeded then 1 else 0),
                    retryCount = Just 0,
                    failureReason = eventResult.failureReason,
                    failedQueryName = eventResult.failedQueryName,
                    queryOutcomes = Nothing,
                    tagStats = Nothing,
                    createdAt = now,
                    updatedAt = now
                  }
            Just existing
              -- Cancelled while this batch was running: the runner stops the run
              -- right after this, so the row must stay Cancelled.
              | existing.status == DKCJR.Cancelled -> pure ()
              | otherwise ->
                QKCJR.updateByPrimaryKey
                  existing
                    { DKCJR.status = status,
                      DKCJR.endedAt = mbEndedAt,
                      DKCJR.batchesProcessed = Just (fromMaybe 0 existing.batchesProcessed + (if batchSucceeded then 1 else 0)),
                      DKCJR.usersProcessed = Just (fromMaybe 0 existing.usersProcessed + batchUsers),
                      DKCJR.retryCount = Just (fromMaybe 0 existing.retryCount + (if existing.status == DKCJR.Failed then 1 else 0)),
                      DKCJR.failureReason = eventResult.failureReason,
                      DKCJR.failedQueryName = eventResult.failedQueryName,
                      DKCJR.updatedAt = now
                    }
        _ -> logWarning "Skipping kaal chakra job run history: merchantId/merchantOperatingCityId unavailable"

-- Closes this phase's row(s) as Cancelled, or writes one if the phase never ran
-- (a job cancelled before its first batch has no row and possibly no eventId yet).
recordChakraJobCancelled ::
  (BeamFlow m r) =>
  Id LYT.Merchant ->
  Id LYT.MerchantOperatingCity ->
  LYT.Chakra ->
  DKCJR.KaalChakraJobRunPhase ->
  Maybe (Id LYT.Event) ->
  Id AnyJob ->
  m ()
recordChakraJobCancelled merchantId merchantOperatingCityId chakra phase mbEventId jobId = do
  now <- getCurrentTime
  existingRows <- maybe (pure []) QKCJR.findAllByEventId mbEventId
  case filter ((== phase) . (.phase)) existingRows of
    [] -> do
      newId <- generateGUID
      eventId <- maybe (Id <$> generateGUID) pure mbEventId
      QKCJR.create
        DKCJR.KaalChakraJobRun
          { id = newId,
            eventId,
            chakra,
            phase,
            schedulerJobId = Just jobId.getId,
            merchantId,
            merchantOperatingCityId,
            status = DKCJR.Cancelled,
            startedAt = now,
            endedAt = Just now,
            usersProcessed = Nothing,
            batchesProcessed = Just 0,
            retryCount = Just 0,
            failureReason = Nothing,
            failedQueryName = Nothing,
            queryOutcomes = Nothing,
            tagStats = Nothing,
            createdAt = now,
            updatedAt = now
          }
    phaseRows ->
      forM_ phaseRows $ \row ->
        QKCJR.updateByPrimaryKey row {DKCJR.status = DKCJR.Cancelled, DKCJR.endedAt = Just now, DKCJR.updatedAt = now}

-- The job was cancelled, deleted or moved while this batch was running: record it,
-- drop the run's state, and return DuplicateExecution so the scheduler neither
-- requeues the job nor overwrites its Cancelled status.
abandonRun ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  Id AnyJob ->
  LYT.Chakra ->
  DKCJR.KaalChakraJobRunPhase ->
  Maybe (Id LYT.Event) ->
  m ExecutionResult
abandonRun h jobId chakra phase mbEventId = do
  logInfo "Kaal Chakra job is no longer active after this batch; stopping the run"
  case (h.merchantId, h.merchantOperatingCityId) of
    (Just merchantId, Just merchantOperatingCityId) ->
      handle (\(err :: SomeException) -> logError $ "Failed to record cancelled kaal chakra run: " <> show err) $
        recordChakraJobCancelled merchantId merchantOperatingCityId chakra phase mbEventId jobId
    _ -> pure ()
  Event.clearRunState h.merchantId h.merchantOperatingCityId chakra mbEventId
  pure DuplicateExecution

-- A job without a city can never apply tags (updateUserTagsHandler needs one), so end it instead of letting it reschedule itself forever
withChakraCity ::
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  LYT.Chakra ->
  m ExecutionResult ->
  m ExecutionResult
withChakraCity h chakra runJob = case h.merchantOperatingCityId of
  Nothing -> do
    logError $ "Terminating " <> show chakra <> " Kaal Chakra job: it has no merchant operating city, so tags cannot be applied"
    pure $ Terminate "Kaal Chakra job has no merchant operating city"
  Just _ -> runJob

runKaalChakraAndRescheduleJob ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  Id AnyJob ->
  m Bool ->
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  m ExecutionResult
runKaalChakraAndRescheduleJob h jobId isStillActive chakra jobData = withChakraCity h chakra $ do
  logInfo $ "Running " <> show chakra <> " Job"
  now <- getCurrentTime
  let req = mkRunKaalChakraJobReq chakra jobData now
  eventResult <- Event.kaalChakraEvent h req
  recordChakraJobRun h jobId now chakra DKCJR.FetchUserData eventResult
  stillActive <- isStillActive
  if not stillActive
    then abandonRun h jobId chakra DKCJR.FetchUserData eventResult.eventId
    else continueFetchRun h chakra jobData req eventResult

continueFetchRun ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  LYT.RunKaalChakraJobReq ->
  LYT.RunKaalChakraJobRes ->
  m ExecutionResult
continueFetchRun h chakra jobData req eventResult = do
  timeAfterRun <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      Event.resetRetryCounter False h.merchantId h.merchantOperatingCityId chakra
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) timeAfterRun
    LYT.Completed -> do
      Event.resetRetryCounter False h.merchantId h.merchantOperatingCityId chakra
      void $ Event.clearEventData False h.merchantId h.merchantOperatingCityId chakra Nothing --- passing Nothing will only clear the batch number key, which is required for the next job from 0 again
      whenJust eventResult.eventId $ \eventId -> do
        let updateUserTagJobData = LYT.mkUpdateTagDataFromKaalChakraJobData req eventId jobData.startTime
        h.createUpdateUserTagDataJob chakra updateUserTagJobData (addUTCTime 60 timeAfterRun) -- starting updateTags job after 60 seconds of caching UserData
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.decrChakraBatchNumber False h.merchantId h.merchantOperatingCityId chakra
      canRetry <- Event.canRetry False h.merchantId h.merchantOperatingCityId chakra
      if canRetry
        then do
          pure $ ReSchedule $ addUTCTime (fromIntegral (10 :: Integer)) timeAfterRun
        else do
          Event.resetRetryCounter False h.merchantId h.merchantOperatingCityId chakra
          let newScheduleTime = getNextChakraTime jobData.startTime timeAfterRun chakra
          h.createFetchUserDataJob chakra jobData newScheduleTime
          pure $ Terminate "Fetch user data job failed"

runKaalChakraUpdateTagsJob ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  Id AnyJob ->
  m Bool ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  m ExecutionResult
runKaalChakraUpdateTagsJob h jobId isStillActive chakra jobData = withChakraCity h chakra $ do
  logInfo $ "Running " <> show chakra <> "UpdateTag Job"
  batchStartedAt <- getCurrentTime
  let req = mkRunKaalChakraUpdateTagJobReq chakra jobData
  eventResult <- Event.updateUserTagsHandler h req
  recordChakraJobRun h jobId batchStartedAt chakra DKCJR.UpdateTags eventResult
  stillActive <- isStillActive
  if not stillActive
    then abandonRun h jobId chakra DKCJR.UpdateTags (Just jobData.eventId)
    else continueUpdateTagsRun h chakra jobData req eventResult

continueUpdateTagsRun ::
  forall m r action.
  Event.ChakraEvent m r action =>
  Event.Handle m action ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  LYT.UpdateKaalBasedTagsJobReq ->
  LYT.RunKaalChakraJobRes ->
  m ExecutionResult
continueUpdateTagsRun h chakra jobData req eventResult = do
  timeAfterRun <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) timeAfterRun
    LYT.Completed -> do
      void $ Event.clearEventData False h.merchantId h.merchantOperatingCityId req.chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime jobData.startTime timeAfterRun req.chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      h.createFetchUserDataJob chakra fetchUserDataJobData newScheduleTime
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData False h.merchantId h.merchantOperatingCityId chakra eventResult.eventId
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
