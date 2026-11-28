{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.Allocator.Jobs.IncentiveJourney.BulkUserCohortMappingUpload
  ( runBulkUserCohortMappingUploadJob,
    defaultBatchSize,
    defaultRescheduleDelaySeconds,
    clampBatchSize,
    maxAttempts,
  )
where

import qualified AWS.S3 as S3
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (FromNamedRecord (..), Header, decodeByName, (.:))
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun as DBulkRun
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRunExtra as QBulkRunExtra
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
  ( AllocatorJobType (..),
    BulkUserCohortMappingUploadJobData (..),
  )
import qualified SharedLogic.FleetDriverSettings as FDS
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaignSync
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.SchedulerJob ()

defaultBatchSize :: Int
defaultBatchSize = 500

maxBatchSize :: Int
maxBatchSize = 2000

defaultRescheduleDelaySeconds :: Int
defaultRescheduleDelaySeconds = 2

-- | App-level retry budget (Pattern B, same idea as FleetEngineRetry). Scheduler
-- jobRetryOnExceptionMap must stay off for this type on RedisBased.
maxAttempts :: Int
maxAttempts = 5

maxErrorMessageLen :: Int
maxErrorMessageLen = 2000

data BulkOperation = OpAssign | OpUnassign
  deriving (Show, Eq)

-- | Assign CSV: userId,cohortMappingId,validTill,enabled
data AssignCSVRow = AssignCSVRow
  { userId :: Text,
    cohortMappingId :: Text,
    validTill :: Text,
    enabled :: Text
  }
  deriving (Show, Eq, Generic)

instance FromNamedRecord AssignCSVRow where
  parseNamedRecord r =
    AssignCSVRow
      <$> r .: "userId"
      <*> r .: "cohortMappingId"
      <*> r .: "validTill"
      <*> r .: "enabled"

-- | Unassign CSV: userId,cohortMappingId only.
data UnassignCSVRow = UnassignCSVRow
  { userId :: Text,
    cohortMappingId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromNamedRecord UnassignCSVRow where
  parseNamedRecord r =
    UnassignCSVRow
      <$> r .: "userId"
      <*> r .: "cohortMappingId"

type BulkUploadJobConstraints m r =
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "s3Env" r (S3.S3Env m)
  )

runBulkUserCohortMappingUploadJob ::
  (BulkUploadJobConstraints m r) =>
  Job 'BulkUserCohortMappingUpload ->
  m ExecutionResult
runBulkUserCohortMappingUploadJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId <> " RunId-" <> jobInfo.jobData.runId) $ do
  let jobData = jobInfo.jobData
      attempt = fromMaybe 1 jobData.attemptCount
      runId = Id jobData.runId
  -- Pattern B (FleetEngineRetry): catch locally, always Complete; never rely on
  -- jobRetryOnExceptionMap (broken for lasting maxErrors on RedisBased).
  result <- try @_ @SomeException $ processChunk id jobData
  case result of
    Right execResult -> pure execResult
    Left err -> do
      let errMsg =
            truncateErrorMessage $
              "attempt="
                <> show attempt
                <> "/"
                <> show maxAttempts
                <> ": "
                <> T.pack (displayException err)
          permanent = isNonRetryableFailure err
      logError $ "BulkUserCohortMappingUpload failed: " <> errMsg
      if permanent || attempt >= maxAttempts
        then do
          safeUpdateProgress runId DBulkRun.Failed (max 0 jobData.offset) Nothing 0 0 (Just id.getId) (Just errMsg)
          logError $
            if permanent
              then
                "BulkUserCohortMappingUpload: non-retryable failure runId="
                  <> jobData.runId
              else
                "BulkUserCohortMappingUpload: exhausted after "
                  <> show attempt
                  <> " attempts runId="
                  <> jobData.runId
          pure Complete
        else do
          let nextAttempt = attempt + 1
              delaySecs = max defaultRescheduleDelaySeconds jobData.rescheduleDelaySeconds
              retryJobData = jobData {attemptCount = Just nextAttempt}
          -- Schedule first: if enqueue fails, do not leave the run stuck in Running.
          scheduled <-
            try @_ @SomeException $
              void $
                JC.createJobIn @_ @'BulkUserCohortMappingUpload
                  (Just jobData.merchantId)
                  (Just jobData.merchantOperatingCityId)
                  (fromIntegral delaySecs)
                  retryJobData
          case scheduled of
            Right _ -> do
              -- Surface last error while still retrying so the dashboard Error column is not empty.
              safeUpdateProgress runId DBulkRun.Running (max 0 jobData.offset) Nothing 0 0 (Just id.getId) (Just errMsg)
              logError $
                "BulkUserCohortMappingUpload: scheduling retry attempt="
                  <> show nextAttempt
                  <> "/"
                  <> show maxAttempts
                  <> " in "
                  <> show delaySecs
                  <> "s"
            Left schedErr -> do
              let failMsg =
                    truncateErrorMessage $
                      errMsg
                        <> "; retry scheduling failed: "
                        <> T.pack (displayException schedErr)
              logError $ "BulkUserCohortMappingUpload: retry scheduling failed: " <> failMsg
              safeUpdateProgress runId DBulkRun.Failed (max 0 jobData.offset) Nothing 0 0 (Just id.getId) (Just failMsg)
          pure Complete

processChunk ::
  (BulkUploadJobConstraints m r) =>
  Id AnyJob ->
  BulkUserCohortMappingUploadJobData ->
  m ExecutionResult
processChunk jobId jobData = do
  let batchSize = clampBatchSize jobData.batchSize
      offset = max 0 jobData.offset
      delaySecs = max 0 jobData.rescheduleDelaySeconds
      runId = Id jobData.runId
      attempt = fromMaybe 1 jobData.attemptCount
      operation = parseOperation jobData.operationType

  logInfo $
    "BulkUserCohortMappingUpload: op="
      <> showOperation operation
      <> " s3="
      <> jobData.s3FilePath
      <> " offset="
      <> show offset
      <> " batchSize="
      <> show batchSize
      <> " attempt="
      <> show attempt

  csvText <- S3.get (T.unpack jobData.s3FilePath)

  (total, processedCount, skipped, distinctUserIds) <-
    case operation of
      OpAssign -> do
        rows <- parseCsvOrThrow csvText
        let total = length rows
            chunk = take batchSize $ drop offset rows
        when (null chunk) $
          logInfo $ "BulkUserCohortMappingUpload: no rows left at offset=" <> show offset <> " total=" <> show total
        safeUpdateProgress runId DBulkRun.Running offset (Just total) 0 0 (Just jobId.getId) Nothing
        (processedCount, skipped, distinctUserIds) <- processAssignRows chunk
        pure (total, processedCount, skipped, distinctUserIds)
      OpUnassign -> do
        rows <- parseUnassignCsvOrThrow csvText
        let total = length rows
            chunk = take batchSize $ drop offset rows
        when (null chunk) $
          logInfo $ "BulkUserCohortMappingUpload: no rows left at offset=" <> show offset <> " total=" <> show total
        safeUpdateProgress runId DBulkRun.Running offset (Just total) 0 0 (Just jobId.getId) Nothing
        (processedCount, skipped, distinctUserIds) <- processUnassignRows chunk
        pure (total, processedCount, skipped, distinctUserIds)

  forM_ distinctUserIds $ \uid ->
    CQAssignment.clearCacheByPersonId IJ.DriverActor uid

  let nextOffset = offset + processedCount + skipped
      hasMore = nextOffset < total
  if hasMore
    then do
      let nextJobData =
            jobData
              { offset = nextOffset,
                batchSize = batchSize,
                rescheduleDelaySeconds = delaySecs,
                attemptCount = Just 1
              }
      void $
        JC.createJobIn @_ @'BulkUserCohortMappingUpload
          (Just jobData.merchantId)
          (Just jobData.merchantOperatingCityId)
          (fromIntegral delaySecs)
          nextJobData
      safeUpdateProgress runId DBulkRun.Running nextOffset (Just total) processedCount skipped (Just jobId.getId) Nothing
      logInfo $
        "BulkUserCohortMappingUpload: chunk done processed="
          <> show processedCount
          <> " skippedRows="
          <> show skipped
          <> " nextOffset="
          <> show nextOffset
          <> "/"
          <> show total
      pure Complete
    else do
      safeUpdateProgress runId DBulkRun.Succeeded nextOffset (Just total) processedCount skipped (Just jobId.getId) Nothing
      logInfo $
        "BulkUserCohortMappingUpload: finished totalRows="
          <> show total
          <> " lastOffset="
          <> show nextOffset
          <> " lastChunkProcessed="
          <> show processedCount
          <> " lastChunkSkipped="
          <> show skipped
      pure Complete

processAssignRows ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  [AssignCSVRow] ->
  m (Int, Int, [Id Common.Person])
processAssignRows chunk = do
  rowResults <- forM chunk $ \row ->
    case ( parseId row.userId,
           parseId row.cohortMappingId,
           parseValidTill row.validTill,
           parseBool row.enabled
         ) of
      (Just userIdText, Just cohortMappingIdText, Just validTill, Just enabled) -> do
        insertedUserId <-
          QUCMExtra.insertUserCohortMappingIgnoringConflict
            (Id userIdText)
            (Id cohortMappingIdText :: Id DCJM.CohortJourneyMapping)
            enabled
            validTill
        pure (Just (Id userIdText), insertedUserId)
      _ -> pure (Nothing, Nothing)
  let parsedUserIds = mapMaybe fst rowResults
      insertedUserIds = mapMaybe snd rowResults
      insertedCount = length insertedUserIds
      skipped = length chunk - insertedCount
      distinctUserIds = List.nub parsedUserIds
  pure (insertedCount, skipped, distinctUserIds)

processUnassignRows ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  [UnassignCSVRow] ->
  m (Int, Int, [Id Common.Person])
processUnassignRows chunk = do
  -- Same efficiency stance as bulk assign: no per-row person/merchant lookup.
  -- Per row: disable UCM + clear IJCampaign tag / AA (same side effects as single unassign).
  rowResults <- forM chunk $ \row ->
    case (parseId row.userId, parseId row.cohortMappingId) of
      (Just userIdText, Just cohortMappingIdText) -> do
        let userId = Id userIdText
            cjmId = Id cohortMappingIdText :: Id DCJM.CohortJourneyMapping
        QUCMExtra.disableUserCohortMapping userId cjmId
        -- Same order as Core.unassignUser: clear assignment cache before fallible
        -- tag/AA cleanup so a mid-row failure does not leave enabled membership cached.
        CQAssignment.clearCacheByPersonId IJ.DriverActor userId
        -- Same as single unassign: AA cleanup only when the CJM tag was present.
        -- Skips pending invites (no tag) so driver-owned AutoAssign is kept; avoids an
        -- extra person read. Rare sticky-AA if cleanup fails after tag clear is accepted.
        matched <- MgCampaignSync.clearMgOptedInOnDriverInfo userId cjmId
        when matched $ FDS.onCampaignMembershipCleared (cast userId) (cast cjmId)
        pure (Just userId)
      _ -> pure Nothing
  let processedUserIds = catMaybes rowResults
      processedCount = length processedUserIds
      skipped = length chunk - processedCount
      distinctUserIds = List.nub processedUserIds
  pure (processedCount, skipped, distinctUserIds)

-- | Never lets progress-table failures abort CSV processing / scheduler chaining.
safeUpdateProgress ::
  (BeamFlow m r) =>
  Id DBulkRun.BulkUserCohortMappingRun ->
  DBulkRun.BulkUserCohortMappingRunStatus ->
  Int ->
  Maybe Int ->
  Int ->
  Int ->
  Maybe Text ->
  Maybe Text ->
  m ()
safeUpdateProgress runId status offset totalRows insertedDelta skippedDelta mbSchedulerJobId mbErrorMessage = do
  result <-
    try @_ @SomeException $
      QBulkRunExtra.updateProgress runId status offset totalRows insertedDelta skippedDelta mbSchedulerJobId mbErrorMessage
  case result of
    Right _ -> pure ()
    Left err ->
      logWarning $
        "BulkUserCohortMappingUpload: progress update failed runId="
          <> getId runId
          <> " err="
          <> T.pack (displayException err)

truncateErrorMessage :: Text -> Text
truncateErrorMessage msg
  | T.length msg <= maxErrorMessageLen = msg
  | otherwise = T.take maxErrorMessageLen msg <> "...(truncated)"

-- | Fail immediately for deterministic errors. Do not treat all S3/IO failures as
-- permanent — only CSV parse (prefix we throw) and explicit object-not-found.
isNonRetryableFailure :: SomeException -> Bool
isNonRetryableFailure err =
  let msg = displayException err
   in "CSV parse failed:" `List.isInfixOf` msg
        || isS3ObjectNotFound msg

isS3ObjectNotFound :: String -> Bool
isS3ObjectNotFound msg =
  "NoSuchKey" `List.isInfixOf` msg
    || ("get-object" `List.isInfixOf` msg && "404" `List.isInfixOf` msg)

clampBatchSize :: Int -> Int
clampBatchSize n
  | n <= 0 = defaultBatchSize
  | n > maxBatchSize = maxBatchSize
  | otherwise = n

parseOperation :: Maybe Text -> BulkOperation
parseOperation = \case
  Just t | T.toLower (T.strip t) == "unassign" -> OpUnassign
  _ -> OpAssign

showOperation :: BulkOperation -> Text
showOperation = \case
  OpAssign -> "Assign"
  OpUnassign -> "Unassign"

parseCsvOrThrow :: (MonadThrow m, Log m) => Text -> m [AssignCSVRow]
parseCsvOrThrow csvText =
  case decodeByName (LBS.fromStrict $ TE.encodeUtf8 csvText) :: Either String (Header, V.Vector AssignCSVRow) of
    Left err -> do
      logError $ "BulkUserCohortMappingUpload: CSV parse error: " <> T.pack err
      throwIO $ userError $ "CSV parse failed: " <> err
    Right (_, v) -> pure (V.toList v)

parseUnassignCsvOrThrow :: (MonadThrow m, Log m) => Text -> m [UnassignCSVRow]
parseUnassignCsvOrThrow csvText =
  case decodeByName (LBS.fromStrict $ TE.encodeUtf8 csvText) :: Either String (Header, V.Vector UnassignCSVRow) of
    Left err -> do
      logError $ "BulkUserCohortMappingUpload: Unassign CSV parse error: " <> T.pack err
      throwIO $ userError $ "CSV parse failed: " <> err
    Right (_, v) -> pure (V.toList v)

-- | Row-level parsers return Maybe so bad cells skip the row without failing the job.
parseId :: Text -> Maybe Text
parseId raw =
  let cleaned = T.strip raw
   in if T.null cleaned then Nothing else Just cleaned

parseValidTill :: Text -> Maybe UTCTime
parseValidTill raw =
  let s = T.unpack (T.strip raw)
      parsers =
        [ parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" s,
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" s,
          parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" s,
          parseTimeM True defaultTimeLocale "%Y-%m-%d" s
        ]
   in asum parsers

parseBool :: Text -> Maybe Bool
parseBool raw =
  case T.toLower (T.strip raw) of
    "true" -> Just True
    "1" -> Just True
    "yes" -> Just True
    "false" -> Just False
    "0" -> Just False
    "no" -> Just False
    _ -> Nothing
