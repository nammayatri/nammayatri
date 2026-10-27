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
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun as DBulkRun
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
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

data UserCohortMappingCSVRow = UserCohortMappingCSVRow
  { userId :: Text,
    cohortMappingId :: Text,
    validTill :: Text,
    enabled :: Text
  }
  deriving (Show, Eq, Generic)

instance FromNamedRecord UserCohortMappingCSVRow where
  parseNamedRecord r =
    UserCohortMappingCSVRow
      <$> r .: "userId"
      <*> r .: "cohortMappingId"
      <*> r .: "validTill"
      <*> r .: "enabled"

runBulkUserCohortMappingUploadJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
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
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Id AnyJob ->
  BulkUserCohortMappingUploadJobData ->
  m ExecutionResult
processChunk jobId jobData = do
  let batchSize = clampBatchSize jobData.batchSize
      offset = max 0 jobData.offset
      delaySecs = max 0 jobData.rescheduleDelaySeconds
      runId = Id jobData.runId
      attempt = fromMaybe 1 jobData.attemptCount

  logInfo $
    "BulkUserCohortMappingUpload: s3="
      <> jobData.s3FilePath
      <> " offset="
      <> show offset
      <> " batchSize="
      <> show batchSize
      <> " attempt="
      <> show attempt

  csvText <- S3.get (T.unpack jobData.s3FilePath)
  rows <- parseCsvOrThrow csvText
  let total = length rows
      chunk = take batchSize $ drop offset rows

  when (null chunk) $
    logInfo $ "BulkUserCohortMappingUpload: no rows left at offset=" <> show offset <> " total=" <> show total

  -- Best-effort: progress table must never fail the upload job.
  safeUpdateProgress runId DBulkRun.Running offset (Just total) 0 0 (Just jobId.getId) Nothing

  rowResults <- forM chunk $ \row ->
    case ( parseId "userId" row.userId,
           parseId "cohortMappingId" row.cohortMappingId,
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
  forM_ distinctUserIds $ \uid ->
    CQAssignment.clearCacheByPersonId IJ.DriverActor uid

  let nextOffset = offset + length chunk
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
      safeUpdateProgress runId DBulkRun.Running nextOffset (Just total) insertedCount skipped (Just jobId.getId) Nothing
      logInfo $
        "BulkUserCohortMappingUpload: chunk done inserted="
          <> show insertedCount
          <> " skippedRows="
          <> show skipped
          <> " nextOffset="
          <> show nextOffset
          <> "/"
          <> show total
      pure Complete
    else do
      safeUpdateProgress runId DBulkRun.Succeeded nextOffset (Just total) insertedCount skipped (Just jobId.getId) Nothing
      logInfo $
        "BulkUserCohortMappingUpload: finished totalRows="
          <> show total
          <> " lastOffset="
          <> show nextOffset
          <> " lastChunkInserted="
          <> show insertedCount
          <> " lastChunkSkipped="
          <> show skipped
      pure Complete

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

parseCsvOrThrow :: (MonadThrow m, Log m) => Text -> m [UserCohortMappingCSVRow]
parseCsvOrThrow csvText =
  case decodeByName (LBS.fromStrict $ TE.encodeUtf8 csvText) :: Either String (Header, V.Vector UserCohortMappingCSVRow) of
    Left err -> do
      logError $ "BulkUserCohortMappingUpload: CSV parse error: " <> T.pack err
      throwIO $ userError $ "CSV parse failed: " <> err
    Right (_, v) -> pure (V.toList v)

-- | Row-level parsers return Maybe so bad cells skip the row without failing the job.
parseId :: Text -> Text -> Maybe Text
parseId _fieldName raw =
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
