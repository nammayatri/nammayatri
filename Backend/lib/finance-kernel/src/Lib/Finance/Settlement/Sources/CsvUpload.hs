module Lib.Finance.Settlement.Sources.CsvUpload
  ( fetchSettlementFile,
    CsvFileSourceConfig (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.List as List
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.External.Settlement.Types (SplitSettlementCustomerType)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo)
import Kernel.Utils.Logging (Log, logDebug)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.ParserTypeMap (resolveSplitCustomerType)
import Lib.Finance.Settlement.Sources.SFTP (SftpFetchMeta (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

data CsvFileSourceConfig = CsvFileSourceConfig
  { s3Bucket :: Text,
    s3Prefix :: Text,
    fileNamePattern :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

fetchSettlementFile ::
  ( BeamFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Text ->
  CsvFileSourceConfig ->
  m (Either Text (LBS.ByteString, SftpFetchMeta, SplitSettlementCustomerType))
fetchSettlementFile merchantId merchantOperatingCityId paymentGatewayName config = do
  landedRows <- QSFI.findAllByPaymentGatewayMerchantCityAndStatus paymentGatewayName merchantId merchantOperatingCityId LANDED
  inProgressRows <- QSFI.findAllByPaymentGatewayMerchantCityAndStatus paymentGatewayName merchantId merchantOperatingCityId IN_PROGRESS
  pendingRows <- QSFI.findAllByPaymentGatewayMerchantCityAndStatus paymentGatewayName merchantId merchantOperatingCityId PENDING
  transientFailedRows <- QSFI.findAllByPaymentGatewayMerchantCityAndStatus paymentGatewayName merchantId merchantOperatingCityId TRANSIENT_FAILED
  let resumeRows = landedRows <> inProgressRows <> pendingRows <> transientFailedRows
  case List.sortOn (.createdAt) resumeRows of
    (row : _) -> do
      logInfo $
        "CsvUpload: resume file=" <> row.fileName
          <> " trackedFileId="
          <> row.id.getId
          <> " lastProcessedIndex="
          <> show row.lastProcessedIndex
      downloadAndSlice config row.id row.fileName (row.lastProcessedIndex + 1)
    [] -> do
      logInfo "CsvUpload: no resumable files; scanning S3 for new CSVs"
      discoverNewCsvFile merchantId merchantOperatingCityId paymentGatewayName config

discoverNewCsvFile ::
  ( BeamFlow m r,
    MonadIO m,
    L.MonadFlow m
  ) =>
  Text ->
  Text ->
  Text ->
  CsvFileSourceConfig ->
  m (Either Text (LBS.ByteString, SftpFetchMeta, SplitSettlementCustomerType))
discoverNewCsvFile merchantId merchantOperatingCityId paymentGatewayName config = do
  eFiles <- listS3CsvFiles config
  case eFiles of
    Left err -> pure $ Left err
    Right [] -> pure $ Left "No CSV files found in S3 bucket"
    Right files -> do
      unprocessed <- filterUnprocessed paymentGatewayName files
      case unprocessed of
        [] -> pure $ Left "All CSV files in S3 already processed"
        (s3Key : _) -> do
          now <- getCurrentTime
          newId <- Id <$> generateGUID
          let row =
                SettlementFileInfo
                  { id = newId,
                    paymentGatewayName,
                    fileName = s3Key,
                    status = LANDED,
                    lastProcessedIndex = -1,
                    retryCount = Just 0,
                    merchantId,
                    merchantOperatingCityId,
                    createdAt = now,
                    updatedAt = now
                  }
          QSFI.create row
          logInfo $ "CsvUpload: created LANDED tracker id=" <> newId.getId <> " s3Key=" <> s3Key
          downloadAndSlice config newId s3Key 0

filterUnprocessed ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Text]
filterUnprocessed gw files = do
  results <- forM files $ \f -> do
    mbExisting <- QSFI.findByPaymentGatewayNameAndFileName gw f
    pure $ case mbExisting of
      Just row | row.status == COMPLETED -> Nothing
      Just row | row.status == FAILED -> Nothing
      Just _ -> Nothing
      Nothing -> Just f
  pure $ catMaybes results

listS3CsvFiles ::
  (MonadIO m, L.MonadFlow m, Log m) =>
  CsvFileSourceConfig ->
  m (Either Text [Text])
listS3CsvFiles CsvFileSourceConfig {..} = do
  let bucketStr = T.unpack s3Bucket
      prefixStr = T.unpack s3Prefix
      patternFilter = fileNamePattern
  logDebug $ "CsvUpload.listS3CsvFiles: bucket=" <> s3Bucket <> " prefix=" <> s3Prefix
  (exitCode, stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "aws"
        [ "s3api",
          "list-objects-v2",
          "--bucket",
          bucketStr,
          "--prefix",
          prefixStr,
          "--query",
          "Contents[].Key",
          "--output",
          "text"
        ]
        ""
  case exitCode of
    ExitFailure code ->
      pure $ Left $ "S3 list-objects-v2 failed (exit " <> show code <> "): " <> T.pack stderr
    ExitSuccess -> do
      let keys = filter (not . T.null) $ T.words (T.pack stdout)
          csvKeys = filter matchesCsvSuffix keys
          filtered = case patternFilter of
            Nothing -> csvKeys
            Just pat -> filter (T.isInfixOf pat) csvKeys
      logDebug $ "CsvUpload.listS3CsvFiles: found " <> show (length filtered) <> " CSV files"
      pure $ Right (List.sort filtered)

matchesCsvSuffix :: Text -> Bool
matchesCsvSuffix name =
  let lower = T.toLower name
   in ".csv" `T.isSuffixOf` lower || ".csv.zip" `T.isSuffixOf` lower

downloadAndSlice ::
  (MonadIO m, L.MonadFlow m, Log m) =>
  CsvFileSourceConfig ->
  Id SettlementFileInfo ->
  Text ->
  Int ->
  m (Either Text (LBS.ByteString, SftpFetchMeta, SplitSettlementCustomerType))
downloadAndSlice CsvFileSourceConfig {..} infoId s3Key startRow = do
  eBytes <- downloadS3Object s3Bucket s3Key
  case eBytes of
    Left err -> pure $ Left err
    Right csvBytes -> do
      let sliced = sliceCsvPayload startRow csvBytes
      case sliced of
        Left err -> pure $ Left err
        Right (chunk, nData) -> do
          let splitTy = resolveSplitCustomerType Nothing s3Key
              meta =
                SftpFetchMeta
                  { trackedFileId = infoId,
                    firstDataRowIndex = startRow,
                    dataRowsDelivered = nData,
                    atomicPull = False
                  }
          logDebug $
            "CsvUpload: file=" <> s3Key
              <> " startRow="
              <> show startRow
              <> " nData="
              <> show nData
          pure $ Right (chunk, meta, splitTy)

downloadS3Object ::
  (MonadIO m, L.MonadFlow m, Log m) =>
  Text ->
  Text ->
  m (Either Text LBS.ByteString)
downloadS3Object bucket key = do
  tmpDir <- liftIO getTemporaryDirectory
  let localPath = tmpDir </> "settlement_csv_" <> T.unpack (T.replace "/" "_" key)
  logDebug $ "CsvUpload.downloadS3Object: bucket=" <> bucket <> " key=" <> key
  (exitCode, _stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "aws"
        [ "s3api",
          "get-object",
          "--bucket",
          T.unpack bucket,
          "--key",
          T.unpack key,
          localPath
        ]
        ""
  case exitCode of
    ExitFailure code ->
      pure $ Left $ "S3 get-object failed (exit " <> show code <> "): " <> T.pack stderr
    ExitSuccess -> do
      contents <- liftIO $ LBS.readFile localPath
      liftIO $ removeFile localPath
      pure $ Right contents

sliceCsvPayload :: Int -> LBS.ByteString -> Either Text (LBS.ByteString, Int)
sliceCsvPayload nextRowIndex bytes =
  let ls = map (LBSC.filter (/= '\r')) (LBSC.split '\n' bytes)
   in case ls of
        [] -> Left "empty CSV payload"
        (hdr : rest) ->
          let restLen = length rest
              start = min nextRowIndex restLen
              chunk = take csvChunkLimit (drop start rest)
              taken = length chunk
              out = LBS.intercalate (LBSC.singleton '\n') (hdr : chunk)
           in Right (out, taken)

csvChunkLimit :: Int
csvChunkLimit = 5000
