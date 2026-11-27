-- | What a signed-in citizen can do with their own applications.
module Lib.Permit.Application
  ( listMine,
    createDraft,
    updateDraft,
    getMine,
    uploadDocument,
    requestUndertakingOtps,
    submit,
    withdraw,
    getPass,
    qrPrefix,
  )
where

import qualified AWS.S3 as S3
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Permit.Api.Types
import Lib.Permit.Auth
import Lib.Permit.Domain.Types.PermitApplication
import Lib.Permit.Domain.Types.PermitApplicationDocument
import Lib.Permit.Domain.Types.PermitApplicationEvent
import Lib.Permit.Otp
import qualified Lib.Permit.Picnic.Rules as Picnic
import Lib.Permit.Picnic.Types
import Lib.Permit.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Permit.Storage.Queries.PermitApplicant as QApplicant
import qualified Lib.Permit.Storage.Queries.PermitApplication as QApplication
import qualified Lib.Permit.Storage.Queries.PermitApplicationDocument as QDocument
import qualified Lib.Permit.Storage.Queries.PermitApplicationDocumentExtra as QDocumentExtra
import qualified Lib.Permit.Storage.Queries.PermitApplicationExtra as QApplicationExtra
import Lib.Permit.Store
import Lib.Permit.Types

maxDrafts, maxDocumentBytes :: Int
maxDrafts = 10
maxDocumentBytes = 5 * 1024 * 1024

qrPrefix :: Text
qrPrefix = "PMT1:"

-- | Another person's application looks exactly like a missing one.
findOwned :: BeamFlow m r => PermitSession -> Id PermitApplication -> m PermitApplication
findOwned session applicationId = do
  application <- findApplication applicationId
  unless (application.applicantId == session.applicantId && application.merchantId == session.merchantId) $
    throwError $ InvalidRequest "Application not found."
  pure application

requireDraft :: BeamFlow m r => PermitApplication -> m ()
requireDraft application =
  unless (application.status == DRAFT) $
    throwError $ InvalidRequest "A submitted application cannot be edited. Withdraw it and apply again."

failWith :: BeamFlow m r => [RuleFailure] -> m ()
failWith failures =
  unless (null failures) $ throwError $ InvalidRequest $ T.intercalate " " (map (.message) failures)

-- | Fields of an application that follow from its picnic details.
data Derived = Derived
  { permitDate :: Day,
    validFrom :: UTCTime,
    validTill :: UTCTime,
    approvalLevel :: ApprovalLevel,
    primaryConflictHash :: Text,
    secondaryConflictHash :: Text
  }

derive :: BeamFlow m r => PicnicDetails -> m Derived
derive details = do
  failWith (Picnic.checkForm details)
  start <- Picnic.departureInstant details & fromMaybeM (InvalidRequest "Departure time must be in HH:MM.")
  end <- Picnic.returnInstant details & fromMaybeM (InvalidRequest "Return time must be in HH:MM.")
  pure
    Derived
      { permitDate = details.travelDate,
        validFrom = start,
        validTill = end,
        approvalLevel = Picnic.approvalLevelFor details,
        primaryConflictHash = sha256Hex (Picnic.normaliseIdentifier details.vehicle.registrationNo),
        secondaryConflictHash = sha256Hex (Picnic.normaliseIdentifier details.driver.licenceNo)
      }

listMine :: (BeamFlow m r, EncFlow m r) => PermitSession -> PermitType -> m PermitApplicationListRes
listMine session wantedType = do
  mine <- QApplication.findAllByApplicantId Nothing Nothing session.applicantId
  items <- forM (filter (\a -> a.permitType == wantedType) mine) $ \application -> do
    (_, details) <- loadDetails application
    pure (listItem application details)
  pure PermitApplicationListRes {applications = items}

createDraft :: (BeamFlow m r, EncFlow m r) => PermitSession -> PermitCreateApplicationReq -> m PermitCreateApplicationRes
createDraft session req = do
  details <- case req.permitType of
    PICNIC -> requirePicnic req.details
  derived <- derive details
  mine <- QApplication.findAllByApplicantId Nothing Nothing session.applicantId
  when (length (filter (\a -> a.status == DRAFT) mine) >= maxDrafts) $
    throwError $ InvalidRequest "Too many drafts. Submit or reuse an existing one."
  applicationId <- generateGUID
  referenceSeed <- generateGUIDText
  now <- getCurrentTime
  encrypted <- encrypt (encodeDetails details)
  let referenceNo = "PMT-" <> T.toUpper (T.take 10 (T.filter (/= '-') referenceSeed))
  QApplication.create
    PermitApplication
      { id = applicationId,
        referenceNo,
        permitType = req.permitType,
        applicantId = session.applicantId,
        merchantId = session.merchantId,
        merchantOperatingCityId = session.merchantOperatingCityId,
        status = DRAFT,
        details = encrypted,
        validFrom = derived.validFrom,
        validTill = derived.validTill,
        permitDate = derived.permitDate,
        primaryConflictHash = Just derived.primaryConflictHash,
        secondaryConflictHash = Just derived.secondaryConflictHash,
        approvalLevel = derived.approvalLevel,
        consentAt = Nothing,
        submittedAt = Nothing,
        decisionReason = Nothing,
        decidedById = Nothing,
        decidedByName = Nothing,
        decidedAt = Nothing,
        qrToken = Nothing,
        createdAt = now,
        updatedAt = now
      }
  recordEvent applicationId APPLICANT session.applicantId.getId Nothing "DRAFT_CREATED" Nothing
  pure PermitCreateApplicationRes {applicationId, referenceNo}

updateDraft ::
  (BeamFlow m r, EncFlow m r, MonadMask m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  PermitSession ->
  Id PermitApplication ->
  PermitUpdateApplicationReq ->
  m APISuccess
updateDraft session applicationId req = withApplicationLock applicationId $ do
  application <- findOwned session applicationId
  requireDraft application
  details <- case application.permitType of
    PICNIC -> requirePicnic req.details
  derived <- derive details
  encrypted <- encrypt (encodeDetails details)
  -- Photos go first: if saving then fails, the citizen only has to upload them again.
  removePhotos session application $
    Picnic.documentsInvalidatedBy
      (application.primaryConflictHash /= Just derived.primaryConflictHash)
      (application.secondaryConflictHash /= Just derived.secondaryConflictHash)
  QApplication.updateByPrimaryKey
    application
      { details = encrypted,
        validFrom = derived.validFrom,
        validTill = derived.validTill,
        permitDate = derived.permitDate,
        primaryConflictHash = Just derived.primaryConflictHash,
        secondaryConflictHash = Just derived.secondaryConflictHash,
        approvalLevel = derived.approvalLevel
      }
  pure Success

removePhotos ::
  (BeamFlow m r, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  PermitSession ->
  PermitApplication ->
  [PermitDocumentType] ->
  m ()
removePhotos session application kinds = do
  stale <- filter (\document -> document.documentType `elem` kinds) <$> findDocuments application.id
  unless (null stale) $ do
    let staleKinds = map (.documentType) stale
    QDocumentExtra.deleteByApplicationIdAndTypes application.id staleKinds
    forM_ stale $ \document -> S3.delete (T.unpack document.s3Path)
    recordEvent application.id APPLICANT session.applicantId.getId Nothing "DOCUMENTS_REMOVED" (Just (T.intercalate ", " (map show staleKinds)))

getMine :: (BeamFlow m r, EncFlow m r) => PermitSession -> Id PermitApplication -> m PermitMyApplicationRes
getMine session applicationId = do
  application <- findOwned session applicationId
  (_, details) <- loadDetails application
  documents <- findDocuments applicationId
  pure
    PermitMyApplicationRes
      { applicationId,
        referenceNo = application.referenceNo,
        permitType = application.permitType,
        status = application.status,
        approvalLevel = application.approvalLevel,
        details = PermitDetails {picnic = Just details},
        documents = map documentInfo documents,
        requiredDocuments = Picnic.requiredDocuments details.vehicle,
        decisionReason = application.decisionReason,
        decidedAt = application.decidedAt
      }

-- | The type comes from the file's own first bytes, never from what the caller claims.
imageExtension :: BS.ByteString -> Maybe (Text, Text)
imageExtension bytes
  | BS.pack [0xFF, 0xD8, 0xFF] `BS.isPrefixOf` bytes = Just ("image/jpeg", "jpg")
  | BS.pack [0x89, 0x50, 0x4E, 0x47] `BS.isPrefixOf` bytes = Just ("image/png", "png")
  | otherwise = Nothing

uploadDocument ::
  (BeamFlow m r, MonadMask m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  PermitSession ->
  Id PermitApplication ->
  PermitDocumentUploadReq ->
  m APISuccess
uploadDocument session applicationId req = do
  limitHits ("upload:" <> session.applicantId.getId) 60 600
  -- Four base64 characters carry three bytes, so oversize input is refused before decoding.
  when (T.length req.imageBase64 > (maxDocumentBytes `div` 3 + 1) * 4) $
    throwError $ InvalidRequest "Photo is larger than 5 MB."
  bytes <-
    (convertFromBase Base64 (TE.encodeUtf8 req.imageBase64) :: Either String BS.ByteString)
      & either (const $ throwError $ InvalidRequest "Photo is not valid base64.") pure
  when (BS.length bytes > maxDocumentBytes) $ throwError $ InvalidRequest "Photo is larger than 5 MB."
  (detectedType, extension) <- imageExtension bytes & fromMaybeM (InvalidRequest "Upload a JPG or PNG photo.")
  withApplicationLock applicationId $ do
    application <- findOwned session applicationId
    requireDraft application
    fileId <- generateGUIDText
    let path =
          "permit/" <> application.merchantOperatingCityId.getId <> "/" <> applicationId.getId <> "/"
            <> show req.documentType
            <> "/"
            <> fileId
            <> "."
            <> extension
        fileHash = sha256Hex req.imageBase64
    S3.put (T.unpack path) req.imageBase64
    existing <- find (\d -> d.documentType == req.documentType) <$> findDocuments applicationId
    now <- getCurrentTime
    case existing of
      Just document -> do
        QDocument.updateByPrimaryKey
          document
            { s3Path = path,
              contentType = detectedType,
              sizeBytes = BS.length bytes,
              fileHash = fileHash,
              validUntil = req.validUntil,
              lifetime = req.lifetime
            }
        S3.delete (T.unpack document.s3Path)
      Nothing -> do
        documentId <- generateGUID
        QDocument.create
          PermitApplicationDocument
            { id = documentId,
              applicationId,
              documentType = req.documentType,
              s3Path = path,
              contentType = detectedType,
              sizeBytes = BS.length bytes,
              fileHash,
              validUntil = req.validUntil,
              lifetime = req.lifetime,
              createdAt = now,
              updatedAt = now
            }
    recordEvent applicationId APPLICANT session.applicantId.getId Nothing "DOCUMENT_UPLOADED" (Just (show req.documentType))
  pure Success

-- | Everything that stops submission: SOP rules and an existing booking of the same
-- vehicle or driver on that date. Also returns the OTP context for this exact version.
submissionState :: (BeamFlow m r, EncFlow m r) => PermitApplication -> m (PicnicDetails, [RuleFailure], Text)
submissionState application = do
  (json, details) <- loadDetails application
  documents <- findDocuments application.id
  now <- getCurrentTime
  clashes <-
    QApplicationExtra.findActiveClashes
      application.merchantId
      application.permitType
      application.permitDate
      application.primaryConflictHash
      application.secondaryConflictHash
  let duplicates =
        [ RuleFailure "DUPLICATE" "§2" "This vehicle or driver already has an application for that date."
          | any (\other -> other.id /= application.id) clashes
        ]
      failures = case application.permitType of
        PICNIC -> Picnic.checkSubmission now details (documentFacts documents)
      otpContext = sha256Hex (json <> T.concat (sort (map (\d -> show d.documentType <> d.fileHash <> show d.validUntil <> show d.lifetime) documents)))
  pure (details, failures <> duplicates, otpContext)

applicantSlot, ownerSlot :: Id PermitApplication -> OtpSlot
applicantSlot applicationId = OtpSlot ("undertaking:" <> applicationId.getId)
ownerSlot applicationId = OtpSlot ("owner:" <> applicationId.getId)

requestUndertakingOtps :: (BeamFlow m r, EncFlow m r) => PermitHost m -> PermitSession -> Id PermitApplication -> m PermitUndertakingOtpRes
requestUndertakingOtps host session applicationId = do
  application <- findOwned session applicationId
  requireDraft application
  (details, failures, otpContext) <- submissionState application
  if not (null failures)
    then pure PermitUndertakingOtpRes {otpSent = False, ruleFailures = failures}
    else do
      limitHits ("undertakingOtp:" <> applicationId.getId) 3 600
      limitHits ("ownerOtp:" <> sha256Hex details.vehicle.ownerMobileNumber) 10 3600
      applicant <- QApplicant.findById session.applicantId >>= fromMaybeM (InternalError "Permit applicant missing")
      applicantMobile <- decrypt applicant.mobileNumber
      issueOtp host (applicantSlot applicationId) otpContext applicant.mobileCountryCode applicantMobile
      issueOtp host (ownerSlot applicationId) otpContext applicant.mobileCountryCode details.vehicle.ownerMobileNumber
      pure PermitUndertakingOtpRes {otpSent = True, ruleFailures = []}

submit :: (BeamFlow m r, EncFlow m r, MonadMask m) => PermitSession -> Id PermitApplication -> PermitSubmitReq -> m APISuccess
submit session applicationId req = withApplicationLock applicationId $ do
  application <- findOwned session applicationId
  requireDraft application
  unless (req.acceptUndertaking && req.ownerCertified && req.dataConsent) $
    throwError $ InvalidRequest "Accept the undertaking, the owner self-certification and the data consent."
  withConflictLocks application $ do
    (_, failures, otpContext) <- submissionState application
    failWith failures
    checkOtp (applicantSlot applicationId) otpContext req.applicantOtp
    checkOtp (ownerSlot applicationId) otpContext req.ownerOtp
    now <- getCurrentTime
    QApplication.updateByPrimaryKey application {status = SUBMITTED, submittedAt = Just now, consentAt = Just now}
    discardOtp (applicantSlot applicationId)
    discardOtp (ownerSlot applicationId)
  recordEvent applicationId APPLICANT session.applicantId.getId Nothing "SUBMITTED" Nothing
  pure Success
  where
    -- Two applications for the same vehicle or driver cannot both pass the duplicate check.
    -- Unique indexes on the table back this up.
    withConflictLocks application action =
      lockOn application.primaryConflictHash $ lockOn application.secondaryConflictHash action
    lockOn mbHash action =
      maybe action (\conflict -> withPermitLock ("permit:conflictLock:" <> conflict) action) mbHash

withdraw :: (BeamFlow m r, MonadMask m) => PermitSession -> Id PermitApplication -> m APISuccess
withdraw session applicationId = withApplicationLock applicationId $ do
  application <- findOwned session applicationId
  unless (application.status `elem` [SUBMITTED, APPROVED]) $ throwError $ InvalidRequest "There is nothing to withdraw."
  QApplication.updateByPrimaryKey application {status = WITHDRAWN}
  recordEvent applicationId APPLICANT session.applicantId.getId Nothing "WITHDRAWN" Nothing
  pure Success

getPass :: (BeamFlow m r, EncFlow m r) => PermitSession -> Id PermitApplication -> m PermitPassRes
getPass session applicationId = do
  application <- findOwned session applicationId
  unless (application.status == APPROVED) $ throwError $ InvalidRequest "A QR code is issued only after approval."
  token <- application.qrToken & fromMaybeM (InternalError "Approved permit has no QR token") >>= decrypt
  (_, details) <- loadDetails application
  pure PermitPassRes {qrText = qrPrefix <> token, pass = passInfo application details}
