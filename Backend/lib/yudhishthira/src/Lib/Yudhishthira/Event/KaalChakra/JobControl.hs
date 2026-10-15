{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- Dashboard control (reschedule/cancel/delete/list) over Kaal Chakra scheduler jobs,
-- plus the guard every chakra job runner goes through. Generic over the app's job
-- type (AllocatorJobType / RiderJobType) so both apps share one implementation.
module Lib.Yudhishthira.Event.KaalChakra.JobControl
  ( ChakraJobDecision (..),
    ChakraJobControl,
    checkChakraJob,
    withChakraJobGuard,
    rescheduleChakraJob,
    cancelChakraJob,
    deleteChakraJob,
    listUpcomingChakraJobs,
  )
where

import Kernel.Beam.Functions (FromTType'')
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QDBJ
import qualified Lib.Scheduler.JobStorageType.DB.Table as BeamST
import qualified Lib.Scheduler.JobStorageType.Redis.Queries as RQ
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import qualified Lib.Yudhishthira.Event.KaalChakra.Internal as Event
import Lib.Yudhishthira.Event.KaalChakra.Jobs (recordChakraJobCancelled)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.Queries.KaalChakraJobRun as QKCJR
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun as DKCJR

type ChakraJobControl m r t =
  ( BeamFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    JobCreator r m,
    JobProcessor t,
    ToJSON t,
    FromTType'' BeamST.SchedulerJob (AnyJob t)
  )

data ChakraJobDecision = RunJob | SkipJob Text
  deriving (Eq, Show)

-- Long enough to outlive any queued copy: Quarterly is the longest cycle (90 days).
-- Cross-app key: written by the dashboard API process, read by the job runner process.
deletedJobMarkerTtl :: Int
deletedJobMarkerTtl = 100 * 24 * 3600

deletedJobKey :: Id AnyJob -> Text
deletedJobKey jobId = "kaal_chakra:deleted_job:" <> jobId.getId

allChakras :: [LYT.Chakra]
allChakras = [LYT.Daily, LYT.Weekly, LYT.Monthly, LYT.Quarterly]

allPhases :: [DKCJR.KaalChakraJobRunPhase]
allPhases = [DKCJR.FetchUserData, DKCJR.UpdateTags]

-- Mirrors the job type constructors both apps declare (Daily, DailyUpdateTag, ...).
chakraJobTypeName :: LYT.Chakra -> DKCJR.KaalChakraJobRunPhase -> Text
chakraJobTypeName chakra DKCJR.FetchUserData = show chakra
chakraJobTypeName chakra DKCJR.UpdateTags = show chakra <> "UpdateTag"

chakraJobTypeNames :: [Text]
chakraJobTypeNames = [chakraJobTypeName chakra phase | chakra <- allChakras, phase <- allPhases]

classifyChakraJobType :: Text -> Maybe (LYT.Chakra, DKCJR.KaalChakraJobRunPhase)
classifyChakraJobType name =
  find (\(chakra, phase) -> chakraJobTypeName chakra phase == name) [(chakra, phase) | chakra <- allChakras, phase <- allPhases]

-- Runner-side check. A queued copy is only executed when it still matches the
-- authoritative scheduler_job row; anything else is skipped as DuplicateExecution,
-- which the scheduler only logs (it does not mark the row Failed).
-- No DB row means the job type is stored in Redis only, so there is nothing to
-- compare against and the job runs.
checkChakraJob ::
  forall t m r.
  (JobMonad r m, JobProcessor t, FromTType'' BeamST.SchedulerJob (AnyJob t)) =>
  Id AnyJob ->
  UTCTime ->
  m ChakraJobDecision
checkChakraJob jobId inHandScheduledAt = do
  mbDeleted :: Maybe Bool <- Hedis.withCrossAppRedis $ Hedis.get (deletedJobKey jobId)
  if isJust mbDeleted
    then pure $ SkipJob "job was deleted"
    else do
      mbJob :: Maybe (AnyJob t) <- QDBJ.findById jobId
      pure $ case mbJob of
        Nothing -> RunJob
        Just (AnyJob job)
          | job.status /= Pending -> SkipJob ("job status is " <> show job.status)
          | abs (diffUTCTime job.scheduledAt inHandScheduledAt) > 1 -> SkipJob "job was rescheduled"
          | otherwise -> RunJob

-- The runner gets an "is this job still active?" action to re-check after each
-- batch, before it requeues itself or creates follow-up jobs.
withChakraJobGuard ::
  forall t m r.
  (JobMonad r m, JobProcessor t, FromTType'' BeamST.SchedulerJob (AnyJob t)) =>
  Id AnyJob ->
  UTCTime ->
  (m Bool -> m ExecutionResult) ->
  m ExecutionResult
withChakraJobGuard jobId scheduledAt runGuardedJob = do
  decision <- checkChakraJob @t jobId scheduledAt
  case decision of
    SkipJob reason -> do
      logInfo $ "Skipping Kaal Chakra job: " <> reason
      pure DuplicateExecution
    RunJob -> runGuardedJob ((== RunJob) <$> checkChakraJob @t jobId scheduledAt)

data ChakraJobRef t = ChakraJobRef
  { refJob :: AnyJob t,
    refJobTypeName :: Text,
    refJobContent :: Text,
    refChakra :: LYT.Chakra,
    refPhase :: DKCJR.KaalChakraJobRunPhase,
    refStatus :: JobStatus,
    refScheduledAt :: UTCTime,
    refMerchantId :: Maybe Text
  }

-- Wrong city and non-chakra jobs both read as "not found", so a caller cannot
-- probe for jobs outside their own city.
findChakraJobForCity ::
  forall t m r.
  ChakraJobControl m r t =>
  Id LYT.MerchantOperatingCity ->
  Id AnyJob ->
  m (Either Text (ChakraJobRef t))
findChakraJobForCity merchantOperatingCityId jobId = do
  mbJob :: Maybe (AnyJob t) <- QDBJ.findById jobId
  pure $ case mbJob of
    Nothing -> Left "Job not found"
    Just anyJob@(AnyJob job@Job {merchantOperatingCityId = jobCityId, merchantId = jobMerchantId}) -> do
      let storedInfo = storeJobInfo job.jobInfo
          inCallerCity = (getId <$> jobCityId) == Just merchantOperatingCityId.getId
      case classifyChakraJobType storedInfo.storedJobType of
        Just (chakra, phase)
          | inCallerCity ->
            Right
              ChakraJobRef
                { refJob = anyJob,
                  refJobTypeName = storedInfo.storedJobType,
                  refJobContent = storedInfo.storedJobContent,
                  refChakra = chakra,
                  refPhase = phase,
                  refStatus = job.status,
                  refScheduledAt = job.scheduledAt,
                  refMerchantId = getId <$> jobMerchantId
                }
        _ -> Left "Job not found"

-- Update-tags jobs carry their run's eventId in the job data; a fetch job's
-- eventId is only known once it has run (from its history rows).
runEventId :: ChakraJobRef t -> [DKCJR.KaalChakraJobRun] -> Maybe (Id LYT.Event)
runEventId ref startedRows = case ref.refPhase of
  DKCJR.UpdateTags -> (.eventId) <$> (decodeFromText ref.refJobContent :: Maybe LYT.UpdateKaalBasedTagsData)
  DKCJR.FetchUserData -> (.eventId) <$> listToMaybe startedRows

rescheduleChakraJob ::
  forall t m r.
  ChakraJobControl m r t =>
  Id LYT.MerchantOperatingCity ->
  Id AnyJob ->
  UTCTime ->
  m (Either Text ())
rescheduleChakraJob merchantOperatingCityId jobId newScheduledAt = do
  now <- getCurrentTime
  if newScheduledAt <= now
    then pure $ Left "scheduledAt must be in the future"
    else
      findChakraJobForCity @t merchantOperatingCityId jobId >>= \case
        Left err -> pure $ Left err
        Right ref
          | ref.refStatus /= Pending -> pure $ Left "Job is not pending; cannot reschedule"
          | otherwise -> do
            startedRows <- QKCJR.findAllBySchedulerJobId jobId.getId
            if not (null startedRows)
              then pure $ Left "Job has already started; cancel it instead"
              else do
                QDBJ.reSchedule jobId newScheduledAt
                schedulerType <- asks (.schedulerType)
                case schedulerType of
                  RedisBased -> do
                    void $ RQ.removeQueuedCopies jobId ref.refScheduledAt
                    RQ.reSchedule ref.refJob newScheduledAt
                  DbBased -> pure ()
                pure $ Right ()

-- Works on a scheduled or a running job. A running job stops at its next batch
-- boundary (the runner re-checks after every batch). The chain to the next cycle
-- is not continued: someone has to schedule the chakra again, and that run starts
-- from batch 0 because the run's state is cleared here.
cancelChakraJob ::
  forall t m r.
  ChakraJobControl m r t =>
  Id LYT.Merchant ->
  Id LYT.MerchantOperatingCity ->
  Id AnyJob ->
  m (Either Text ())
cancelChakraJob callerMerchantId merchantOperatingCityId jobId =
  findChakraJobForCity @t merchantOperatingCityId jobId >>= \case
    Left err -> pure $ Left err
    Right ref
      | ref.refStatus /= Pending -> pure $ Left "Job is not pending; nothing to cancel"
      | otherwise -> do
        QDBJ.markAsCancelled jobId
        void $ RQ.removeQueuedCopies jobId ref.refScheduledAt
        startedRows <- QKCJR.findAllBySchedulerJobId jobId.getId
        let mbEventId = runEventId ref startedRows
            jobMerchantId = maybe callerMerchantId Id ref.refMerchantId
        -- A fetch job that never ran owns no run state yet; clearing here could
        -- reset the counters of another run of this chakra in the same city.
        when (not (null startedRows) || ref.refPhase == DKCJR.UpdateTags) $
          Event.clearRunState (Just jobMerchantId) (Just merchantOperatingCityId) ref.refChakra mbEventId
        recordChakraJobCancelled jobMerchantId merchantOperatingCityId ref.refChakra ref.refPhase mbEventId jobId
        pure $ Right ()

-- Only for a job that has not started. The marker is written before the job is
-- removed so a copy firing concurrently (or already on the stream) is skipped.
deleteChakraJob ::
  forall t m r.
  ChakraJobControl m r t =>
  Id LYT.Merchant ->
  Id LYT.MerchantOperatingCity ->
  Id AnyJob ->
  m (Either Text ())
deleteChakraJob callerMerchantId merchantOperatingCityId jobId =
  findChakraJobForCity @t merchantOperatingCityId jobId >>= \case
    Left err -> pure $ Left err
    Right ref
      | ref.refStatus /= Pending -> pure $ Left "Only a pending job can be deleted"
      | otherwise -> do
        startedRows <- QKCJR.findAllBySchedulerJobId jobId.getId
        if not (null startedRows)
          then pure $ Left "Job has already started; cancel it instead"
          else do
            Hedis.withCrossAppRedis $ Hedis.setExp (deletedJobKey jobId) True deletedJobMarkerTtl
            void $ QAllJ.deleteJob ref.refJobTypeName jobId ref.refScheduledAt
            -- Deleting a pending update-tags job abandons its run, whose fetched
            -- user data would otherwise be left behind.
            when (ref.refPhase == DKCJR.UpdateTags) $ do
              let jobMerchantId = maybe callerMerchantId Id ref.refMerchantId
              Event.clearRunState (Just jobMerchantId) (Just merchantOperatingCityId) ref.refChakra (runEventId ref [])
            pure $ Right ()

listUpcomingChakraJobs ::
  forall t m r.
  ChakraJobControl m r t =>
  Id LYT.MerchantOperatingCity ->
  m LYT.ChakraUpcomingJobsRes
listUpcomingChakraJobs merchantOperatingCityId = do
  pendingJobs :: [AnyJob t] <- QDBJ.findPendingByTypesAndCity chakraJobTypeNames merchantOperatingCityId.getId (Just 100)
  pure LYT.ChakraUpcomingJobsRes {jobs = mapMaybe toUpcomingJob pendingJobs}
  where
    toUpcomingJob :: AnyJob t -> Maybe LYT.ChakraUpcomingJob
    toUpcomingJob (AnyJob job) = do
      (chakra, phase) <- classifyChakraJobType (storeJobInfo job.jobInfo).storedJobType
      pure LYT.ChakraUpcomingJob {jobId = job.id.getId, chakra, phase, scheduledAt = job.scheduledAt}
