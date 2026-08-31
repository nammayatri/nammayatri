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
import qualified Domain.Types.Person as DP
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
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
import qualified Storage.CachedQueries.IncentiveJourneyAssignment as CQAssignment

defaultBatchSize :: Int
defaultBatchSize = 500

maxBatchSize :: Int
maxBatchSize = 2000

defaultRescheduleDelaySeconds :: Int
defaultRescheduleDelaySeconds = 2

data UserCohortMappingCSVRow = UserCohortMappingCSVRow
  { userId :: Text,
    cohortMappingId :: Text,
    validTill :: Text,
    isTestGroup :: Text
  }
  deriving (Show, Eq, Generic)

instance FromNamedRecord UserCohortMappingCSVRow where
  parseNamedRecord r =
    UserCohortMappingCSVRow
      <$> r .: "userId"
      <*> r .: "cohortMappingId"
      <*> r .: "validTill"
      <*> r .: "isTestGroup"

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
      batchSize = clampBatchSize jobData.batchSize
      offset = max 0 jobData.offset
      delaySecs = max 0 jobData.rescheduleDelaySeconds

  logInfo $
    "BulkUserCohortMappingUpload: s3="
      <> jobData.s3FilePath
      <> " offset="
      <> show offset
      <> " batchSize="
      <> show batchSize

  csvText <- S3.get (T.unpack jobData.s3FilePath)
  rows <- parseCsvOrThrow csvText
  let total = length rows
      chunk = take batchSize $ drop offset rows

  when (null chunk) $
    logInfo $ "BulkUserCohortMappingUpload: no rows left at offset=" <> show offset <> " total=" <> show total

  insertedUserIds <- fmap catMaybes $
    forM chunk $ \row ->
      case ( parseId "userId" row.userId,
             parseId "cohortMappingId" row.cohortMappingId,
             parseValidTill row.validTill,
             parseBool row.isTestGroup
           ) of
        (Just userIdText, Just cohortMappingIdText, Just validTill, Just isTestGroup) ->
          QUCMExtra.insertUserCohortMappingIgnoringConflict
            (Id userIdText)
            (Id cohortMappingIdText :: Id DCJM.CohortJourneyMapping)
            isTestGroup
            validTill
        _ -> pure Nothing

  let insertedCount = length insertedUserIds
      distinctUserIds = List.nub insertedUserIds
      skipped = length chunk - insertedCount
  forM_ distinctUserIds $ \uid ->
    CQAssignment.clearCacheByPersonId (cast uid :: Id DP.Person)

  let nextOffset = offset + length chunk
      hasMore = nextOffset < total
  if hasMore
    then do
      let nextJobData =
            jobData
              { offset = nextOffset,
                batchSize = batchSize,
                rescheduleDelaySeconds = delaySecs
              }
      void $
        JC.createJobIn @_ @'BulkUserCohortMappingUpload
          (Just jobData.merchantId)
          (Just jobData.merchantOperatingCityId)
          (fromIntegral delaySecs)
          nextJobData
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
      throwError (InternalError $ "CSV parse failed: " <> T.pack err)
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
