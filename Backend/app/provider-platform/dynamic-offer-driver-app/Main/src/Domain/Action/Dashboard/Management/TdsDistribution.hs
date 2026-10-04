-- | TDS certificate (Form 16A) disbursement: an admin uploads one quarter's certificate folder for a city.
--
--   create   : parse every file name (PAN_Qx_FY.pdf), store one tds_distribution_pdf_file row per file and hand out a
--              presigned S3 PUT URL for each file that passed the name checks; the browser uploads straight to S3
--   validate : check each uploaded object in S3 and match its PAN to a driver or fleet owner of the city
--   review   : batch counts + the file list (All / Ready / Issues)
--   cancel   : discard a batch that has not been sent (also "Replace folder")
--   confirm  : one tds_distribution_record per ready file, then the ScheduledTDSDistribution job emails them
--   report   : recipients with their delivery status, retry of failed ones; page 1 summary and upload list;
--              the person profile tab (certificates per quarter, download, single resend)
module Domain.Action.Dashboard.Management.TdsDistribution
  ( postTdsDistributionBatch,
    postTdsDistributionBatchValidate,
    getTdsDistributionBatch,
    getTdsDistributionBatchFiles,
    postTdsDistributionBatchCancel,
    postTdsDistributionBatchConfirm,
    getTdsDistributionBatchRecords,
    postTdsDistributionBatchRetryFailed,
    getTdsDistributionBatches,
    getTdsDistributionSummary,
    getTdsDistributionPersonCertificates,
    getTdsDistributionRecordDownloadUrl,
    postTdsDistributionRecordRetry,
  )
where

import qualified API.Types.ProviderPlatform.Management.TdsDistribution as Common
import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import Data.Either (lefts, rights)
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time (Day, fromGregorian, utctDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TDSDistributionBatch as DTB
import qualified Domain.Types.TDSDistributionPdfFile as DTF
import qualified Domain.Types.TDSDistributionRecord as DTR
import Domain.Utils (mapConcurrently)
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.TdsDistribution as STD
import qualified SharedLogic.TdsDistribution.Delivery as Delivery
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TDSDistributionBatch as QBatch
import qualified Storage.Queries.TDSDistributionBatchExtra as QBatchExtra
import qualified Storage.Queries.TDSDistributionPdfFile as QFile
import qualified Storage.Queries.TDSDistributionRecord as QRecord

maxFilesPerBatch :: Int
maxFilesPerBatch = 500

maxBatchBytes :: Int
maxBatchBytes = 200 * 1024 * 1024

-- | SES raw messages are capped at 10 MB and base64 grows an attachment by ~37%, so ~7 MB of PDF fits.
maxFileBytes :: Int
maxFileBytes = 7 * 1024 * 1024

-- | Files checked in parallel during validation.
validationParallelism :: Int
validationParallelism = 20

-- | Issues found from the file name and declared size when the batch is created. They cannot change on
-- re-validation, and such files are never uploaded.
createStageIssues :: [DTF.TDSFileIssue]
createStageIssues = [DTF.INVALID_NAME, DTF.NOT_PDF, DTF.WRONG_QUARTER, DTF.WRONG_FY, DTF.TOO_LARGE]

postTdsDistributionBatch :: ShortId DM.Merchant -> Context.City -> Text -> Common.CreateTdsBatchReq -> Flow Common.CreateTdsBatchResp
postTdsDistributionBatch merchantShortId opCity requestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  unless (STD.isValidFinancialYear req.financialYear) $
    throwError (InvalidRequest "financialYear must look like 2026-27")
  when (null req.files) $
    throwError (InvalidRequest "The folder has no files")
  when (length req.files > maxFilesPerBatch) $
    throwError (InvalidRequest $ "A folder can have at most " <> show maxFilesPerBatch <> " files")
  when (sum ((.sizeBytes) <$> req.files) > maxBatchBytes) $
    throwError (InvalidRequest "A folder can be at most 200 MB")

  batchId <- generateGUID
  now <- getCurrentTime
  let quarter = quarterToText req.quarter
  QBatch.create
    DTB.TDSDistributionBatch
      { id = batchId,
        merchantId = merchant.id,
        merchantOperatingCityId = merchantOpCityId,
        financialYear = req.financialYear,
        quarter,
        folderName = req.folderName,
        status = DTB.DRAFT,
        totalFiles = length req.files,
        uploadedById = requestorId,
        uploadedByName = req.uploadedByName,
        validatedAt = Nothing,
        confirmedAt = Nothing,
        confirmedById = Nothing,
        confirmedByName = Nothing,
        completedAt = Nothing,
        createdAt = now,
        updatedAt = now
      }

  pathPrefix <- asks (.s3Env.pathPrefix)
  let batchPath = pathPrefix <> "/tds-certificates/" <> merchant.id.getId <> "/" <> merchantOpCityId.getId <> "/" <> req.financialYear <> "/" <> quarter <> "/" <> batchId.getId <> "/"
  results <- forM req.files $ \fileInput -> do
    fileId <- generateGUID
    let issue = createStageIssue req.financialYear quarter fileInput
        s3FilePath = batchPath <> fileId.getId <> ".pdf"
    QFile.create
      DTF.TDSDistributionPdfFile
        { id = fileId,
          tdsDistributionRecordId = Nothing,
          s3FilePath,
          fileName = fileInput.fileName,
          batchId = Just batchId,
          sizeBytes = Just fileInput.sizeBytes,
          validationStatus = Just $ if isJust issue then DTF.SKIPPED else DTF.PENDING,
          issue,
          matchedPersonId = Nothing,
          recipientType = Nothing,
          createdAt = now,
          updatedAt = now
        }
    case issue of
      Just fileIssue ->
        pure $ Right Common.TdsRejectedFile {fileId = fileId.getId, fileName = fileInput.fileName, issue = issueToApi fileIssue}
      Nothing -> do
        uploadUrl <- S3.generateUploadUrl (T.unpack s3FilePath) merchant.mediaFileDocumentLinkExpires
        pure $ Left Common.TdsFileUpload {fileId = fileId.getId, fileName = fileInput.fileName, uploadUrl}
  pure
    Common.CreateTdsBatchResp
      { batchId = batchId.getId,
        uploads = lefts results,
        rejected = rights results
      }

createStageIssue :: Text -> Text -> Common.TdsFileInput -> Maybe DTF.TDSFileIssue
createStageIssue financialYear quarter fileInput =
  case STD.parseTdsFileName fileInput.fileName of
    Nothing -> Just DTF.INVALID_NAME
    Just parsed
      | not (STD.isPdfFile fileInput.fileName fileInput.mimeType) -> Just DTF.NOT_PDF
      | parsed.quarter /= quarter -> Just DTF.WRONG_QUARTER
      | parsed.financialYear /= financialYear -> Just DTF.WRONG_FY
      | fileInput.sizeBytes > maxFileBytes -> Just DTF.TOO_LARGE
      | otherwise -> Nothing

-- | Outcome of checking one file during validation.
data FileCheck = FileCheck
  { issue :: Maybe DTF.TDSFileIssue,
    matched :: Maybe (Id DP.Person, DTF.TDSRecipientType),
    sizeBytes :: Maybe Int
  }

postTdsDistributionBatchValidate :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TdsBatchResp
postTdsDistributionBatchValidate merchantShortId opCity batchIdText = do
  (merchantOpCityId, scopedBatch) <- getBatchInScope merchantShortId opCity batchIdText
  withUnsentBatchLock scopedBatch.id "This batch has already been sent or cancelled" $ \batch -> do
    files <- QFile.findAllByBatchId (Just batch.id)
    checked <- fmap concat . forM (chunksOf validationParallelism files) $ \chunk ->
      mapConcurrently (\file -> (file.id,) <$> try @_ @SomeException (checkFile merchantOpCityId file)) chunk
    -- mapConcurrently drops a file whose check threw; fail the whole request rather than leave it half-checked
    let failures = [(fileId, err) | (fileId, Left err) <- checked]
    unless (null failures && length checked == length files) $ do
      forM_ failures $ \(fileId, err) -> logError $ "TDS batch " <> batch.id.getId <> ": check failed for file " <> fileId.getId <> ": " <> show err
      throwError (InternalError "Could not check all files, please validate again")
    let checks = M.fromList [(fileId, check) | (fileId, Right check) <- checked]
    let duplicatePans = findDuplicatePans files checks
    alreadySent <- findAlreadySent batch checks
    forM_ files $ \file -> whenJust (M.lookup file.id checks) $ \check -> do
      let finalIssue =
            check.issue
              <|> (if maybe False (`elem` duplicatePans) (filePan file) then Just DTF.DUPLICATE_PAN else Nothing)
              <|> (if maybe False ((`elem` alreadySent) . fst) check.matched then Just DTF.ALREADY_SENT else Nothing)
      QFile.updateValidationResult
        (Just $ if isJust finalIssue then DTF.SKIPPED else DTF.READY)
        finalIssue
        (fst <$> check.matched)
        (snd <$> check.matched)
        check.sizeBytes
        file.id
    now <- getCurrentTime
    QBatch.updateStatusAndValidatedAt DTB.VALIDATED (Just now) batch.id
  buildBatchResp scopedBatch.id

-- | Check one file: for files already rejected by name only find who the PAN belongs to (for display);
-- otherwise check the S3 object and match the PAN.
checkFile :: Id DMOC.MerchantOperatingCity -> DTF.TDSDistributionPdfFile -> Flow FileCheck
checkFile merchantOpCityId file
  | Just knownIssue <- file.issue,
    knownIssue `elem` createStageIssues = do
    matched <- maybe (pure Nothing) (fmap matchedPerson . STD.matchPan merchantOpCityId) (filePan file)
    pure FileCheck {issue = Just knownIssue, matched, sizeBytes = file.sizeBytes}
  | otherwise = do
    mbObject <- (Just <$> S3.headRequest (T.unpack file.s3FilePath)) `catch` \(_ :: SomeException) -> pure Nothing
    case mbObject of
      Nothing -> pure FileCheck {issue = Just DTF.MISSING_UPLOAD, matched = Nothing, sizeBytes = file.sizeBytes}
      Just object -> do
        let actualSize = fromInteger object.fileSizeInBytes
        if actualSize > maxFileBytes
          then pure FileCheck {issue = Just DTF.TOO_LARGE, matched = Nothing, sizeBytes = Just actualSize}
          else do
            panMatch <- maybe (pure STD.PanNotFound) (STD.matchPan merchantOpCityId) (filePan file)
            let issue = case panMatch of
                  STD.PanMatched {} -> Nothing
                  STD.PanNotFound -> Just DTF.PAN_NOT_FOUND
                  STD.PanAmbiguous -> Just DTF.AMBIGUOUS_PAN
            pure FileCheck {issue, matched = matchedPerson panMatch, sizeBytes = Just actualSize}
  where
    matchedPerson = \case
      STD.PanMatched person recipientType -> Just (person.id, recipientType)
      _ -> Nothing

-- | PANs that more than one otherwise-sendable file of the batch carries; all their copies are skipped.
findDuplicatePans :: [DTF.TDSDistributionPdfFile] -> M.Map (Id DTF.TDSDistributionPdfFile) FileCheck -> [Text]
findDuplicatePans files checks =
  M.keys . M.filter (> (1 :: Int)) $
    M.fromListWith (+) [(pan, 1) | file <- files, Just check <- [M.lookup file.id checks], isNothing check.issue, Just pan <- [filePan file]]

-- | Matched people whose certificate for this quarter was already sent, or is being sent by another batch.
findAlreadySent :: DTB.TDSDistributionBatch -> M.Map (Id DTF.TDSDistributionPdfFile) FileCheck -> Flow [Id DP.Person]
findAlreadySent batch checks = do
  let candidates = L.nub [personId | check <- M.elems checks, isNothing check.issue, Just (personId, _) <- [check.matched]]
      isTakenThisQuarter record =
        record.status `elem` takenStatuses
          && record.quarter == batch.quarter
          && isForFinancialYear batch.financialYear record
  fmap catMaybes . forM candidates $ \personId -> do
    records <- QRecord.findAllByDriverId (Just personId)
    pure $ if any isTakenThisQuarter records then Just personId else Nothing

-- | A person's certificate for a quarter in these statuses is not uploaded again.
takenStatuses :: [DTR.TDSDistributionStatus]
takenStatuses = [DTR.PENDING, DTR.SENDING, DTR.SENT, DTR.DELIVERED]

-- | Records of the financial year, including legacy rows that carry only the assessment year.
isForFinancialYear :: Text -> DTR.TDSDistributionRecord -> Bool
isForFinancialYear financialYear record =
  record.financialYear == Just financialYear
    || (isNothing record.financialYear && Just record.assessmentYear == STD.assessmentYearOf financialYear)

getTdsDistributionBatch :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TdsBatchResp
getTdsDistributionBatch merchantShortId opCity batchIdText = do
  (_, batch) <- getBatchInScope merchantShortId opCity batchIdText
  buildBatchResp batch.id

getTdsDistributionBatchFiles ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Common.TdsFileFilter ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.TdsFileListResp
getTdsDistributionBatchFiles merchantShortId opCity batchIdText mbFilter mbSearch mbLimit mbOffset = do
  (_, batch) <- getBatchInScope merchantShortId opCity batchIdText
  files <- QFile.findAllByBatchId (Just batch.id)
  names <- fmap M.fromList . forM (L.nub $ mapMaybe (.matchedPersonId) files) $ \personId ->
    (personId,) . fmap STD.personDisplayName <$> QPerson.findById personId
  let items = toFileItem names <$> L.sortOn fileSortKey files
      search = T.toLower . T.strip <$> mbSearch
      filtered = filter (\item -> matchesFilter item && matchesSearch search item) items
      limit = min maxFilesPerBatch $ fromMaybe 50 mbLimit
      offset = fromMaybe 0 mbOffset
  pure
    Common.TdsFileListResp
      { files = take limit $ drop offset filtered,
        totalCount = length filtered
      }
  where
    matchesFilter item = case fromMaybe Common.FILTER_ALL mbFilter of
      Common.FILTER_ALL -> True
      Common.FILTER_READY -> item.status == Common.READY
      Common.FILTER_ISSUES -> item.status == Common.SKIPPED
    matchesSearch Nothing _ = True
    matchesSearch (Just "") _ = True
    matchesSearch (Just needle) item =
      any (T.isInfixOf needle . T.toLower) (item.fileName : catMaybes [item.pan, item.matchedName])
    -- files with issues first, then by name
    fileSortKey file = (file.validationStatus /= Just DTF.SKIPPED, file.fileName)

postTdsDistributionBatchCancel :: ShortId DM.Merchant -> Context.City -> Text -> Flow APISuccess
postTdsDistributionBatchCancel merchantShortId opCity batchIdText = do
  (_, scopedBatch) <- getBatchInScope merchantShortId opCity batchIdText
  withUnsentBatchLock scopedBatch.id "Only a batch that has not been sent can be cancelled" $ \batch -> do
    QBatch.updateStatus DTB.CANCELLED batch.id
    files <- QFile.findAllByBatchId (Just batch.id)
    let uploaded = filter (\file -> maybe True (`notElem` createStageIssues) file.issue) files
    forM_ (chunksOf validationParallelism uploaded) $ \chunk ->
      void . flip mapConcurrently chunk $ \file ->
        S3.delete (T.unpack file.s3FilePath) `catch` \(err :: SomeException) ->
          logWarning $ "TDS batch " <> batch.id.getId <> ": could not delete " <> file.s3FilePath <> ": " <> show err
  pure Success

getOpCityId :: ShortId DM.Merchant -> Context.City -> Flow (Id DMOC.MerchantOperatingCity)
getOpCityId merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

getBatchInScope :: ShortId DM.Merchant -> Context.City -> Text -> Flow (Id DMOC.MerchantOperatingCity, DTB.TDSDistributionBatch)
getBatchInScope merchantShortId opCity batchIdText = do
  merchantOpCityId <- getOpCityId merchantShortId opCity
  batch <- QBatch.findById (Id batchIdText) >>= fromMaybeM (InvalidRequest "TDS batch not found")
  unless (batch.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest "TDS batch not found")
  pure (merchantOpCityId, batch)

-- | Validate and cancel both rewrite the batch's files; never let them run at the same time. The batch is
-- re-read under the lock, so a batch cancelled (or sent) by a concurrent request is not acted on.
withUnsentBatchLock :: Id DTB.TDSDistributionBatch -> Text -> (DTB.TDSDistributionBatch -> Flow a) -> Flow a
withUnsentBatchLock batchId notUnsentMessage action = do
  let lockKey = "TdsDistribution:Batch:Lock:" <> batchId.getId
  gotLock <- Redis.tryLockRedis lockKey 300
  unless gotLock $ throwError (InvalidRequest "This batch is being updated, please try again in a moment")
  flip finally (Redis.unlockRedis lockKey) $ do
    batch <- QBatch.findById batchId >>= fromMaybeM (InvalidRequest "TDS batch not found")
    unless (batch.status `elem` [DTB.DRAFT, DTB.VALIDATED]) $
      throwError (InvalidRequest notUnsentMessage)
    action batch

buildBatchResp :: Id DTB.TDSDistributionBatch -> Flow Common.TdsBatchResp
buildBatchResp batchId = do
  batch <- QBatch.findById batchId >>= fromMaybeM (InvalidRequest "TDS batch not found")
  files <- QFile.findAllByBatchId (Just batch.id)
  counts <- recordCounts <$> QRecord.findAllByBatchId (Just batch.id)
  let ready = filter ((== Just DTF.READY) . (.validationStatus)) files
      skipped = filter ((== Just DTF.SKIPPED) . (.validationStatus)) files
      isPanIssue file = file.issue `elem` [Just DTF.PAN_NOT_FOUND, Just DTF.AMBIGUOUS_PAN]
  pure
    Common.TdsBatchResp
      { batchId = batch.id.getId,
        financialYear = batch.financialYear,
        quarter = fromMaybe Common.Q1 (quarterFromText batch.quarter),
        folderName = batch.folderName,
        status = batchStatusToApi batch.status,
        totalFiles = batch.totalFiles,
        parsedFiles = length files - length (filter ((== Just DTF.PENDING) . (.validationStatus)) files),
        readyCount = length ready,
        driverCount = length $ filter ((== Just DTF.DRIVER) . (.recipientType)) ready,
        fleetOwnerCount = length $ filter ((== Just DTF.FLEET_OWNER) . (.recipientType)) ready,
        panNotFoundCount = length $ filter isPanIssue skipped,
        invalidCount = length $ filter (not . isPanIssue) skipped,
        uploadedByName = batch.uploadedByName,
        createdAt = batch.createdAt,
        validatedAt = batch.validatedAt,
        confirmedAt = batch.confirmedAt,
        confirmedByName = batch.confirmedByName,
        completedAt = batch.completedAt,
        pendingCount = counts.pending,
        sentCount = counts.sent,
        deliveredCount = counts.delivered,
        failedCount = counts.failed
      }

toFileItem :: M.Map (Id DP.Person) (Maybe Text) -> DTF.TDSDistributionPdfFile -> Common.TdsFileItem
toFileItem names file =
  Common.TdsFileItem
    { fileId = file.id.getId,
      fileName = file.fileName,
      pan = filePan file,
      matchedName = file.matchedPersonId >>= join . (`M.lookup` names),
      recipientType = recipientTypeToApi <$> file.recipientType,
      status = case file.validationStatus of
        Just DTF.READY -> Common.READY
        Just DTF.SKIPPED -> Common.SKIPPED
        _ -> Common.PENDING,
      issue = issueToApi <$> file.issue
    }

filePan :: DTF.TDSDistributionPdfFile -> Maybe Text
filePan file = (.pan) <$> STD.parseTdsFileName file.fileName

quarterToText :: Common.TdsQuarter -> Text
quarterToText = \case
  Common.Q1 -> "Q1"
  Common.Q2 -> "Q2"
  Common.Q3 -> "Q3"
  Common.Q4 -> "Q4"

quarterFromText :: Text -> Maybe Common.TdsQuarter
quarterFromText = \case
  "Q1" -> Just Common.Q1
  "Q2" -> Just Common.Q2
  "Q3" -> Just Common.Q3
  "Q4" -> Just Common.Q4
  _ -> Nothing

batchStatusToApi :: DTB.TDSDistributionBatchStatus -> Common.TdsBatchStatus
batchStatusToApi = \case
  DTB.DRAFT -> Common.DRAFT
  DTB.VALIDATED -> Common.VALIDATED
  DTB.SENDING -> Common.SENDING
  DTB.COMPLETED -> Common.COMPLETED
  DTB.CANCELLED -> Common.CANCELLED

recipientTypeToApi :: DTF.TDSRecipientType -> Common.TdsRecipientType
recipientTypeToApi = \case
  DTF.DRIVER -> Common.DRIVER
  DTF.FLEET_OWNER -> Common.FLEET_OWNER

issueToApi :: DTF.TDSFileIssue -> Common.TdsFileIssue
issueToApi = \case
  DTF.INVALID_NAME -> Common.INVALID_NAME
  DTF.NOT_PDF -> Common.NOT_PDF
  DTF.WRONG_QUARTER -> Common.WRONG_QUARTER
  DTF.WRONG_FY -> Common.WRONG_FY
  DTF.TOO_LARGE -> Common.TOO_LARGE
  DTF.MISSING_UPLOAD -> Common.MISSING_UPLOAD
  DTF.DUPLICATE_PAN -> Common.DUPLICATE_PAN
  DTF.PAN_NOT_FOUND -> Common.PAN_NOT_FOUND
  DTF.AMBIGUOUS_PAN -> Common.AMBIGUOUS_PAN
  DTF.ALREADY_SENT -> Common.ALREADY_SENT

-- | Create (or reuse, after a failed send) one record per ready file and start emailing them.
postTdsDistributionBatchConfirm :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Common.TdsConfirmReq -> Flow Common.TdsBatchResp
postTdsDistributionBatchConfirm merchantShortId opCity batchIdText requestorId req = do
  (merchantOpCityId, scopedBatch) <- getBatchInScope merchantShortId opCity batchIdText
  withUnsentBatchLock scopedBatch.id "This batch has already been sent or cancelled" $ \batch -> do
    unless (batch.status == DTB.VALIDATED) $
      throwError (InvalidRequest "Validate the batch before sending it")
    files <- QFile.findAllByBatchId (Just batch.id)
    let ready = [(file, personId) | file <- files, file.validationStatus == Just DTF.READY, Just personId <- [file.matchedPersonId]]
    when (null ready) $
      throwError (InvalidRequest "No certificates in this batch are ready to send")
    now <- getCurrentTime
    forM_ ready $ \(file, personId) -> do
      let isThisQuarter record = record.quarter == batch.quarter && record.financialYear == Just batch.financialYear
      existing <- find isThisQuarter <$> QRecord.findAllByDriverId (Just personId)
      case existing of
        Just record
          | record.status `elem` takenStatuses ->
            -- sent or queued by another batch since this one was validated
            QFile.updateValidationResult (Just DTF.SKIPPED) (Just DTF.ALREADY_SENT) file.matchedPersonId file.recipientType file.sizeBytes file.id
        Just record -> do
          QRecord.updateForResend (Just batch.id) DTR.PENDING 0 Nothing record.id
          QFile.updateTdsDistributionRecordId (Just record.id) file.id
        Nothing -> do
          recordId <- generateGUID
          QRecord.create
            DTR.TDSDistributionRecord
              { id = recordId,
                driverId = Just personId,
                emailAddress = Nothing,
                fileName = Nothing,
                assessmentYear = fromMaybe batch.financialYear (STD.assessmentYearOf batch.financialYear),
                financialYear = Just batch.financialYear,
                quarter = batch.quarter,
                status = DTR.PENDING,
                retryCount = 0,
                batchId = Just batch.id,
                failureReason = Nothing,
                latestEmailDeliveryId = Nothing,
                attemptCount = Just 0,
                lastAttemptAt = Nothing,
                deliveredAt = Nothing,
                merchantId = batch.merchantId,
                merchantOperatingCityId = batch.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
          QFile.updateTdsDistributionRecordId (Just recordId) file.id
    QBatch.updateConfirmed DTB.SENDING (Just now) (Just requestorId) req.confirmedByName batch.id
    startBatchJob batch.merchantId merchantOpCityId batch.id
  buildBatchResp scopedBatch.id

startBatchJob :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DTB.TDSDistributionBatch -> Flow ()
startBatchJob merchantId merchantOpCityId batchId =
  createJobIn @_ @'ScheduledTDSDistribution (Just merchantId) (Just merchantOpCityId) 0 $
    ScheduledTDSDistributionJobData
      { merchantId,
        merchantOperatingCityId = merchantOpCityId,
        batchSize = Nothing,
        batchId = Just batchId
      }

getTdsDistributionBatchRecords ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Common.TdsRecordFilter ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.TdsRecordListResp
getTdsDistributionBatchRecords merchantShortId opCity batchIdText mbFilter mbSearch mbLimit mbOffset = do
  (_, batch) <- getBatchInScope merchantShortId opCity batchIdText
  records <- QRecord.findAllByBatchId (Just batch.id)
  files <- QFile.findAllByBatchId (Just batch.id)
  persons <- loadPersons (mapMaybe (.driverId) records)
  let filesByRecord = latestFileByRecord files
      items = toRecordItem persons filesByRecord <$> L.sortOn recordSortKey records
      search = T.toLower . T.strip <$> mbSearch
      filtered = filter (\item -> matchesFilter item && matchesSearch search item) items
      limit = min maxFilesPerBatch $ fromMaybe 50 mbLimit
      offset = fromMaybe 0 mbOffset
  pure
    Common.TdsRecordListResp
      { records = take limit $ drop offset filtered,
        totalCount = length filtered
      }
  where
    matchesFilter item = case fromMaybe Common.REC_FILTER_ALL mbFilter of
      Common.REC_FILTER_ALL -> True
      Common.REC_FILTER_DELIVERED -> item.status `elem` [Common.REC_SENT, Common.REC_DELIVERED]
      Common.REC_FILTER_FAILED -> item.status == Common.REC_FAILED
      Common.REC_FILTER_PENDING -> item.status `elem` [Common.REC_PENDING, Common.REC_SENDING]
    matchesSearch Nothing _ = True
    matchesSearch (Just "") _ = True
    matchesSearch (Just needle) item =
      any (T.isInfixOf needle . T.toLower) (catMaybes [item.name, item.pan, item.email, item.fileName])
    -- failures first, then the order the records were created in
    recordSortKey record = (record.status /= DTR.FAILED, record.createdAt)

-- | Queue every failed recipient of a sent batch again.
postTdsDistributionBatchRetryFailed :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow APISuccess
postTdsDistributionBatchRetryFailed merchantShortId opCity batchIdText _requestorId = do
  (merchantOpCityId, batch) <- getBatchInScope merchantShortId opCity batchIdText
  unless (batch.status `elem` [DTB.SENDING, DTB.COMPLETED]) $
    throwError (InvalidRequest "Only a batch that has been sent can be retried")
  records <- QRecord.findAllByBatchId (Just batch.id)
  let failedRecords = filter (isFailedStatus . (.status)) records
  when (null failedRecords) $
    throwError (InvalidRequest "This batch has no failed recipients")
  forM_ failedRecords $ \record ->
    QRecord.updateStatusAndRetryCount DTR.PENDING 0 record.id
  QBatch.updateStatus DTB.SENDING batch.id
  startBatchJob batch.merchantId merchantOpCityId batch.id
  pure Success

-- | Recent uploads of the city, newest first.
getTdsDistributionBatches :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.TdsBatchListResp
getTdsDistributionBatches merchantShortId opCity mbLimit mbOffset = do
  merchantOpCityId <- getOpCityId merchantShortId opCity
  batches <- QBatchExtra.findAllActiveByCity merchantOpCityId Nothing (Just . min 50 $ fromMaybe 10 mbLimit) mbOffset
  items <- forM batches $ \batch -> do
    counts <- recordCounts <$> QRecord.findAllByBatchId (Just batch.id)
    pure
      Common.TdsBatchListItem
        { batchId = batch.id.getId,
          financialYear = batch.financialYear,
          quarter = fromMaybe Common.Q1 (quarterFromText batch.quarter),
          status = batchStatusToApi batch.status,
          uploadedByName = batch.uploadedByName,
          createdAt = batch.createdAt,
          confirmedAt = batch.confirmedAt,
          sentCount = counts.sent,
          deliveredCount = counts.delivered,
          failedCount = counts.failed
        }
  pure Common.TdsBatchListResp {batches = items}

-- | The four quarter cards of a financial year.
getTdsDistributionSummary :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TdsSummaryResp
getTdsDistributionSummary merchantShortId opCity financialYear = do
  fyStart <- STD.financialYearStart financialYear & fromMaybeM (InvalidRequest "financialYear must look like 2026-27")
  merchantOpCityId <- getOpCityId merchantShortId opCity
  batches <- QBatchExtra.findAllActiveByCity merchantOpCityId (Just financialYear) Nothing Nothing
  now <- getCurrentTime
  let todayIst = utctDay (addUTCTime (5.5 * 3600) now)
  quarters <- forM [Common.Q1, Common.Q2, Common.Q3, Common.Q4] $ \quarter -> do
    let quarterBatches = filter ((== quarterToText quarter) . (.quarter)) batches -- newest first
        sentBatches = filter ((`elem` [DTB.SENDING, DTB.COMPLETED]) . (.status)) quarterBatches
        (quarterEnd, dueDate) = quarterDates fyStart quarter
    counts <- sumCounts <$> mapM (fmap recordCounts . QRecord.findAllByBatchId . Just . (.id)) sentBatches
    let quarterState
          | any ((== DTB.SENDING) . (.status)) sentBatches = Common.QS_IN_PROGRESS
          | not (null sentBatches) = Common.QS_SENT
          | todayIst > quarterEnd = Common.QS_READY_TO_UPLOAD
          | otherwise = Common.QS_UPCOMING
    pure
      Common.TdsQuarterSummary
        { quarter,
          state = quarterState,
          dueDate,
          latestBatchId = (.id.getId) <$> listToMaybe quarterBatches,
          uploadedAt = (\batch -> fromMaybe batch.createdAt batch.confirmedAt) <$> listToMaybe sentBatches,
          sentCount = counts.sent,
          deliveredCount = counts.delivered,
          failedCount = counts.failed
        }
  pure Common.TdsSummaryResp {financialYear, quarters}

-- | Last day of the quarter and the date Form 16A is due to deductees (15 days after the quarterly return).
quarterDates :: Int -> Common.TdsQuarter -> (Day, Day)
quarterDates fyStart = \case
  Common.Q1 -> (fromGregorian year 6 30, fromGregorian year 8 15)
  Common.Q2 -> (fromGregorian year 9 30, fromGregorian year 11 15)
  Common.Q3 -> (fromGregorian year 12 31, fromGregorian (year + 1) 2 15)
  Common.Q4 -> (fromGregorian (year + 1) 3 31, fromGregorian (year + 1) 6 15)
  where
    year = toInteger fyStart

-- | Driver / fleet owner profile tab: the person's certificate for each quarter of the financial year.
getTdsDistributionPersonCertificates :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow Common.TdsPersonCertificatesResp
getTdsDistributionPersonCertificates merchantShortId opCity personIdText financialYear = do
  unless (STD.isValidFinancialYear financialYear) $
    throwError (InvalidRequest "financialYear must look like 2026-27")
  merchantOpCityId <- getOpCityId merchantShortId opCity
  person <- QPerson.findById (Id personIdText) >>= fromMaybeM (PersonNotFound personIdText)
  unless (person.merchantOperatingCityId == merchantOpCityId) $
    throwError (PersonNotFound personIdText)
  records <- filter (isForFinancialYear financialYear) <$> QRecord.findAllByDriverId (Just person.id)
  certificates <- forM [Common.Q1, Common.Q2, Common.Q3, Common.Q4] $ \quarter -> do
    let mbRecord = listToMaybe . L.sortOn (Down . (.updatedAt)) $ filter ((== quarterToText quarter) . (.quarter)) records
    certificate <- forM mbRecord $ \record -> do
      mbFile <- Delivery.latestPdfFile record.id
      pure $ toRecordItem (M.singleton person.id person) (maybe M.empty (M.singleton record.id) mbFile) record
    pure Common.TdsCertificateItem {quarter, certificate}
  pure
    Common.TdsPersonCertificatesResp
      { personId = person.id.getId,
        email = person.email,
        certificates
      }

getTdsDistributionRecordDownloadUrl :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TdsDownloadUrlResp
getTdsDistributionRecordDownloadUrl merchantShortId opCity recordIdText = do
  (_, record) <- getRecordInScope merchantShortId opCity recordIdText
  pdfFile <- Delivery.latestPdfFile record.id >>= fromMaybeM (InvalidRequest "This record has no certificate file")
  url <- S3.generateDownloadUrl (T.unpack pdfFile.s3FilePath) (Seconds 300)
  pure Common.TdsDownloadUrlResp {url, fileName = pdfFile.fileName}

-- | Resend one failed certificate now, optionally to a corrected address that can also be saved on the profile.
postTdsDistributionRecordRetry :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Common.TdsRetryReq -> Flow Common.TdsRecordItem
postTdsDistributionRecordRetry merchantShortId opCity recordIdText requestorId req = do
  (merchantOpCityId, record) <- getRecordInScope merchantShortId opCity recordIdText
  unless (isFailedStatus record.status) $
    throwError (InvalidRequest "Only a certificate that failed to send can be resent")
  let mbEmail = mfilter (not . T.null) (T.strip <$> req.email)
  whenJust mbEmail $ \email ->
    unless (isPlausibleEmail email) $ throwError (InvalidRequest "Enter a valid email address")
  when req.saveToProfile $
    whenJust ((,) <$> record.driverId <*> mbEmail) $ \(personId, email) -> QPerson.updateEmailByPersonId personId email
  settings <- Delivery.getTdsEmailSettings merchantOpCityId
  updated <- Delivery.sendRecordCertificate settings mbEmail (Just requestorId) False record
  whenJust updated.batchId Delivery.refreshBatchCompletion
  mbFile <- Delivery.latestPdfFile updated.id
  persons <- loadPersons (maybeToList updated.driverId)
  pure $ toRecordItem persons (maybe M.empty (M.singleton updated.id) mbFile) updated
  where
    isPlausibleEmail email = case T.splitOn "@" email of
      [localPart, domain] -> not (T.null localPart) && "." `T.isInfixOf` domain
      _ -> False

getRecordInScope :: ShortId DM.Merchant -> Context.City -> Text -> Flow (Id DMOC.MerchantOperatingCity, DTR.TDSDistributionRecord)
getRecordInScope merchantShortId opCity recordIdText = do
  merchantOpCityId <- getOpCityId merchantShortId opCity
  record <- QRecord.findById (Id recordIdText) >>= fromMaybeM (InvalidRequest "TDS record not found")
  unless (record.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest "TDS record not found")
  pure (merchantOpCityId, record)

isFailedStatus :: DTR.TDSDistributionStatus -> Bool
isFailedStatus status = status `elem` [DTR.FAILED, DTR.MISSING_FILE, DTR.MISSING_MANIFEST, DTR.MISMATCH]

data RecordCounts = RecordCounts {pending :: Int, sent :: Int, delivered :: Int, failed :: Int}

recordCounts :: [DTR.TDSDistributionRecord] -> RecordCounts
recordCounts records =
  RecordCounts
    { pending = count (Delivery.isPendingStatus . (.status)),
      sent = count ((== DTR.SENT) . (.status)),
      delivered = count ((== DTR.DELIVERED) . (.status)),
      failed = count (isFailedStatus . (.status))
    }
  where
    count predicate = length (filter predicate records)

sumCounts :: [RecordCounts] -> RecordCounts
sumCounts = L.foldl' add (RecordCounts 0 0 0 0)
  where
    add a b = RecordCounts (a.pending + b.pending) (a.sent + b.sent) (a.delivered + b.delivered) (a.failed + b.failed)

loadPersons :: [Id DP.Person] -> Flow (M.Map (Id DP.Person) DP.Person)
loadPersons personIds =
  M.fromList . catMaybes <$> forM (L.nub personIds) (\personId -> fmap (personId,) <$> QPerson.findById personId)

-- | Each record's most recently uploaded file.
latestFileByRecord :: [DTF.TDSDistributionPdfFile] -> M.Map (Id DTR.TDSDistributionRecord) DTF.TDSDistributionPdfFile
latestFileByRecord files =
  M.fromListWith newer [(recordId, file) | file <- files, Just recordId <- [file.tdsDistributionRecordId]]
  where
    newer a b = if a.createdAt >= b.createdAt then a else b

toRecordItem ::
  M.Map (Id DP.Person) DP.Person ->
  M.Map (Id DTR.TDSDistributionRecord) DTF.TDSDistributionPdfFile ->
  DTR.TDSDistributionRecord ->
  Common.TdsRecordItem
toRecordItem persons filesByRecord record =
  Common.TdsRecordItem
    { recordId = record.id.getId,
      personId = (.getId) <$> record.driverId,
      name = STD.personDisplayName <$> mbPerson,
      recipientType = recipientTypeToApi <$> ((mbFile >>= (.recipientType)) <|> (mbPerson >>= STD.recipientTypeForRole . (.role))),
      pan = mbFile >>= filePan,
      fileName = (.fileName) <$> mbFile,
      email = record.emailAddress <|> (mbPerson >>= (.email)),
      status = recordStatusToApi record.status,
      failureReason =
        if record.status == DTR.MISSING_FILE
          then Just Common.MISSING_PDF
          else if isFailedStatus record.status then failureReasonToApi <$> record.failureReason else Nothing,
      attemptCount = fromMaybe 0 record.attemptCount,
      lastAttemptAt = record.lastAttemptAt,
      deliveredAt = record.deliveredAt
    }
  where
    mbPerson = record.driverId >>= (`M.lookup` persons)
    mbFile = M.lookup record.id filesByRecord

recordStatusToApi :: DTR.TDSDistributionStatus -> Common.TdsRecordStatus
recordStatusToApi = \case
  DTR.PENDING -> Common.REC_PENDING
  DTR.SENDING -> Common.REC_SENDING
  DTR.SENT -> Common.REC_SENT
  DTR.DELIVERED -> Common.REC_DELIVERED
  DTR.FAILED -> Common.REC_FAILED
  DTR.MISSING_FILE -> Common.REC_FAILED
  DTR.MISSING_MANIFEST -> Common.REC_FAILED
  DTR.MISMATCH -> Common.REC_FAILED

failureReasonToApi :: DTR.TDSFailureReason -> Common.TdsFailureReason
failureReasonToApi = \case
  DTR.MISSING_EMAIL -> Common.MISSING_EMAIL
  DTR.ADDRESS_NOT_FOUND -> Common.ADDRESS_NOT_FOUND
  DTR.MAILBOX_FULL -> Common.MAILBOX_FULL
  DTR.ATTACHMENT_TOO_LARGE -> Common.ATTACHMENT_TOO_LARGE
  DTR.REJECTED -> Common.REJECTED
  DTR.SUPPRESSED -> Common.SUPPRESSED
  DTR.TIMEOUT -> Common.TIMEOUT
  DTR.SEND_ERROR -> Common.SEND_ERROR
