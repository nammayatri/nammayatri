-- | What officials can do. The caller has already checked the dashboard capability
-- and the official's access to the merchant and city.
module Lib.Permit.Review
  ( Official (..),
    Scope (..),
    listForReview,
    getForReview,
    getDocument,
    decide,
    verifyPass,
  )
where

import qualified AWS.S3 as S3
import qualified Crypto.Random as Random
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import qualified Data.ByteString as BS
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
import Lib.Permit.Application (qrPrefix)
import Lib.Permit.Domain.Types.Common
import Lib.Permit.Domain.Types.PermitApplication
import Lib.Permit.Domain.Types.PermitApplicationDocument
import Lib.Permit.Domain.Types.PermitApplicationEvent
import qualified Lib.Permit.Picnic.Rules as Picnic
import Lib.Permit.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Permit.Storage.Queries.PermitApplication as QApplication
import qualified Lib.Permit.Storage.Queries.PermitApplicationEvent as QEvent
import qualified Lib.Permit.Storage.Queries.PermitApplicationExtra as QApplicationExtra
import Lib.Permit.Store
import Lib.Permit.Types

data Official = Official
  { officialId :: Text,
    officialName :: Text
  }

-- | The merchant and operating city named in the dashboard request path.
data Scope = Scope
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }

maxPageSize, maxReasonLength :: Int
maxPageSize = 50
maxReasonLength = 500

inScope :: Scope -> PermitApplication -> Bool
inScope scope application =
  application.merchantId == scope.merchantId
    && application.merchantOperatingCityId == scope.merchantOperatingCityId
    && application.status /= DRAFT

-- | Drafts and other cities' applications look missing.
findReviewable :: BeamFlow m r => Scope -> Id PermitApplication -> m PermitApplication
findReviewable scope applicationId = do
  application <- findApplication applicationId
  unless (inScope scope application) $ throwError $ InvalidRequest "Application not found."
  pure application

listForReview ::
  (BeamFlow m r, EncFlow m r) =>
  Scope ->
  PermitType ->
  Maybe PermitStatus ->
  Maybe Day ->
  Maybe Int ->
  Maybe Int ->
  m PermitApplicationListRes
listForReview scope wantedType mbStatus mbDate mbLimit mbOffset = do
  when (mbStatus == Just DRAFT) $ throwError $ InvalidRequest "Drafts are visible only to the applicant."
  let pageSize = max 1 (min maxPageSize (fromMaybe 20 mbLimit))
      offset = max 0 (fromMaybe 0 mbOffset)
      statuses = maybe [SUBMITTED, APPROVED, REJECTED, WITHDRAWN] pure mbStatus
  found <- QApplicationExtra.findAllForReview scope.merchantOperatingCityId wantedType statuses mbDate pageSize offset
  items <- forM (filter (inScope scope) found) $ \application -> do
    (_, details) <- loadDetails application
    pure (listItem application details)
  pure PermitApplicationListRes {applications = items}

getForReview :: (BeamFlow m r, EncFlow m r) => Scope -> Id PermitApplication -> m PermitApplicationDetailsRes
getForReview scope applicationId = do
  application <- findReviewable scope applicationId
  (_, details) <- loadDetails application
  documents <- findDocuments applicationId
  events <- QEvent.findAllByApplicationId Nothing Nothing applicationId
  now <- getCurrentTime
  let failures = case application.permitType of
        PICNIC
          | application.status == SUBMITTED -> Picnic.checkApproval now details (documentFacts documents)
          | otherwise -> []
      decision = do
        reason <- application.decisionReason
        decidedByName <- application.decidedByName
        decidedAt <- application.decidedAt
        -- A withdrawn pass keeps its decision, and only approved passes have a QR token.
        pure PermitDecisionInfo {approved = isJust application.qrToken, reason, decidedByName, decidedAt}
  pure
    PermitApplicationDetailsRes
      { applicationId,
        referenceNo = application.referenceNo,
        permitType = application.permitType,
        status = application.status,
        approvalLevel = application.approvalLevel,
        permitDate = application.permitDate,
        validFrom = application.validFrom,
        validTill = application.validTill,
        submittedAt = application.submittedAt,
        details = PermitDetails {picnic = Just details},
        documents = map documentInfo documents,
        ruleFailures = failures,
        decision,
        history =
          map
            ( \event ->
                PermitHistoryItem
                  { action = event.action,
                    actorType = event.actorType,
                    actorName = event.actorName,
                    reason = event.reason,
                    createdAt = event.createdAt
                  }
            )
            events
      }

getDocument ::
  (BeamFlow m r, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Scope ->
  Official ->
  Id PermitApplication ->
  PermitDocumentType ->
  m PermitDocumentRes
getDocument scope official applicationId wanted = do
  _ <- findReviewable scope applicationId
  document <-
    find (\d -> d.documentType == wanted) <$> findDocuments applicationId
      >>= fromMaybeM (InvalidRequest "No photo of that type was uploaded.")
  imageBase64 <- S3.get (T.unpack document.s3Path)
  recordEvent applicationId OFFICIAL official.officialId (Just official.officialName) "DOCUMENT_VIEWED" (Just (show wanted))
  pure PermitDocumentRes {contentType = document.contentType, imageBase64}

-- | 'level' is the approval level the called endpoint may decide. Each of the two
-- decision endpoints has its own capability, which is how only the District
-- Commissioner decides 60 to 90 minute trips.
decide ::
  (BeamFlow m r, EncFlow m r, MonadMask m) =>
  ApprovalLevel ->
  Scope ->
  Official ->
  Id PermitApplication ->
  PermitDecisionReq ->
  m APISuccess
decide level scope official applicationId req = do
  let reason = T.strip req.reason
  when (T.null reason) $ throwError $ InvalidRequest "Give a reason."
  when (T.length reason > maxReasonLength) $ throwError $ InvalidRequest "The reason is too long."
  withApplicationLock applicationId $ do
    application <- findReviewable scope applicationId
    unless (application.status == SUBMITTED) $ throwError $ InvalidRequest "Only a submitted application can be decided."
    unless (application.approvalLevel == level) $
      throwError . InvalidRequest $ case application.approvalLevel of
        DC -> "Only the District Commissioner can decide this trip."
        STANDARD -> "Use the standard decision for this trip."
    now <- getCurrentTime
    qrToken <-
      if req.approve
        then do
          (_, details) <- loadDetails application
          documents <- findDocuments applicationId
          let failures = case application.permitType of
                PICNIC -> Picnic.checkApproval now details (documentFacts documents)
          unless (null failures) $
            throwError $ InvalidRequest $ "This cannot be approved. " <> T.intercalate " " (map (.message) failures)
          bytes <- liftIO (Random.getRandomBytes 32 :: IO BS.ByteString)
          Just <$> encrypt (TE.decodeUtf8 (convertToBase Base64URLUnpadded bytes))
        else pure Nothing
    QApplication.updateByPrimaryKey
      application
        { status = if req.approve then APPROVED else REJECTED,
          decisionReason = Just reason,
          decidedById = Just official.officialId,
          decidedByName = Just official.officialName,
          decidedAt = Just now,
          qrToken = qrToken
        }
    recordEvent applicationId OFFICIAL official.officialId (Just official.officialName) (if req.approve then "APPROVED" else "REJECTED") (Just reason)
  pure Success

verifyPass :: (BeamFlow m r, EncFlow m r) => Scope -> Official -> PermitVerifyPassReq -> m PermitVerifyPassRes
verifyPass scope official req = do
  (found, scannedFromQr) <- case (nonBlank req.qrText, nonBlank req.referenceNo) of
    (Just qrText, Nothing) -> do
      match <- case T.stripPrefix qrPrefix qrText of
        Nothing -> pure Nothing
        Just token -> getDbHash token >>= QApplicationExtra.findByQrTokenHash
      pure (match, True)
    (Nothing, Just referenceNo) -> do
      match <- QApplication.findByReferenceNo (T.toUpper referenceNo)
      pure (match, False)
    _ -> throwError $ InvalidRequest "Send either the QR text or a reference number."
  now <- getCurrentTime
  case find (inScope scope) found of
    Nothing ->
      pure
        PermitVerifyPassRes
          { result = RED,
            reasons = [ScanReason "UNKNOWN_PASS" "No permit matches. Travel is not permitted on this code or number."],
            intensifiedCheck = False,
            pass = Nothing
          }
    Just application -> do
      (_, details) <- loadDetails application
      let (colour, reasons, intensifiedCheck) = case application.permitType of
            PICNIC -> Picnic.evaluateScan Picnic.ScanInput {now, status = application.status, details, scannedFromQr}
      recordEvent application.id OFFICIAL official.officialId (Just official.officialName) "PASS_CHECKED" (Just (show colour))
      pure PermitVerifyPassRes {result = colour, reasons, intensifiedCheck, pass = Just (passInfo application details)}
  where
    nonBlank = mfilter (not . T.null) . fmap T.strip
