{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.PullDocument
  ( pullDocuments,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, diffDays, parseTimeM, utctDay)
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DLModule
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption (Encrypted, decrypt)
import qualified Kernel.External.SharedLogic.DigiLocker.Error as DigiLockerError
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.Types as VerificationTypes
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Digilocker as DigilockerLockerShared
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverLicenseExtra as QDLE
import qualified Storage.Queries.Person as PersonQuery
import Tools.Error
import qualified Tools.Verification as Verification

pullDocuments ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  APITypes.PullDocumentReq ->
  Flow APISuccess
pullDocuments (mbDriverId, merchantId, merchantOpCityId) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "PullDocument - Starting pull for DriverId: " <> driverId.getId <> ", DocType: " <> show req.docType

  person <- PersonQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  transporterConfig <-
    CQTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $ do
    logError $ "DigiLocker pull document - DigiLocker not enabled for merchantOpCityId: " <> person.merchantOperatingCityId.getId
    throwError DigiLockerNotEnabled

  latestSession <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  session <- case latestSession of
    [] -> throwError DigiLockerNoActiveSession
    (s : _) -> return s

  let sessionMerchantId = session.merchantId
  let sessionMerchantOpCityId = session.merchantOperatingCityId

  when (sessionMerchantId /= merchantId || sessionMerchantOpCityId /= merchantOpCityId) $ do
    logError $ "PullDocument - Mismatched merchant context. Request: (" <> merchantId.getId <> ", " <> merchantOpCityId.getId <> "), Session: (" <> sessionMerchantId.getId <> ", " <> sessionMerchantOpCityId.getId <> ")"
    throwError DigiLockerMismatchedMerchantContext

  unless (req.docType == DVC.DriverLicense) $
    throwError $ DigiLockerUnsupportedDocumentType (show req.docType)

  verifySessionActive session req.docType

  let accessTokenEncryptedMaybe :: Maybe (Encrypted Text) = session.accessToken
  accessTokenEncrypted <- accessTokenEncryptedMaybe & fromMaybeM DigiLockerMissingAccessToken
  accessToken <- decrypt accessTokenEncrypted

  digiLockerConfig <- DigilockerLockerShared.getDigiLockerConfig sessionMerchantOpCityId

  let orgId = digiLockerConfig.dlOrgId
  let pullReq =
        DigiTypes.DigiLockerPullDrivingLicenseReq
          { accessToken = accessToken,
            orgid = orgId,
            doctype = "DRVLC",
            consent = "Y",
            dlno = req.drivingLicenseNumber
          }

  mbPullResp <-
    ( do
        logInfo $ "PullDocument - Calling DigiLocker pull API with session merchant: " <> sessionMerchantId.getId <> ", opCity: " <> sessionMerchantOpCityId.getId
        pullResp <- Verification.pullDigiLockerDrivingLicense sessionMerchantId sessionMerchantOpCityId pullReq
        logInfo $ "PullDocument - Successfully pulled DL document, URI: " <> pullResp.uri
        return $ Just pullResp
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to pull DL: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ DigiLockerOperationFailed $ "Failed to pull driving license from DigiLocker: " <> errorDesc

  pullResp <- mbPullResp & fromMaybeM (InternalError "Pull response is Nothing")

  mbExtractedResp <-
    ( do
        logInfo $ "PullDocument - Fetching and extracting DL data with session merchant: " <> sessionMerchantId.getId <> ", opCity: " <> sessionMerchantOpCityId.getId
        let extractReq =
              DigiTypes.DigiLockerExtractDLReq
                { accessToken = accessToken,
                  uri = pullResp.uri
                }
        extractedResp <- Verification.fetchAndExtractVerifiedDL sessionMerchantId sessionMerchantOpCityId extractReq
        logInfo $ "PullDocument - Successfully extracted DL data"
        return $ Just extractedResp
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to extract DL: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ DigiLockerOperationFailed $ "Failed to extract driving license data: " <> errorDesc

  extractedResp <- mbExtractedResp & fromMaybeM (InternalError "Extracted response is Nothing")

  mbPdfBytes <-
    ( do
        logInfo $ "PullDocument - Fetching PDF from DigiLocker with session merchant: " <> sessionMerchantId.getId <> ", opCity: " <> sessionMerchantOpCityId.getId
        let fileReq =
              DigiTypes.DigiLockerGetFileReq
                { accessToken = accessToken,
                  uri = pullResp.uri
                }
        pdfBytes <- Verification.getDigiLockerFile sessionMerchantId sessionMerchantOpCityId fileReq
        logInfo $ "PullDocument - Successfully fetched PDF"
        return $ Just pdfBytes
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to fetch PDF: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ DigiLockerOperationFailed $ "Failed to fetch driving license PDF: " <> errorDesc

  pdfBytes <- mbPdfBytes & fromMaybeM (InternalError "PDF bytes is Nothing")

  ( do
      logInfo $ "PullDocument - Verifying and storing DL data in database"
      verifyAndStoreDL session person pdfBytes extractedResp
      logInfo $ "PullDocument - Successfully stored DL in database"

      updateDocStatusField session.id req.docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "200") (Just "Driver License verified and stored successfully via pull")
      logInfo $ "PullDocument - Successfully completed pull operation for DriverId: " <> driverId.getId
    )
    `catch` \(err :: DigiLockerError.DigiLockerError) -> do
      let (errorCode, errorDesc) = extractDigiLockerError err
      logError $ "PullDocument - Verification failed: " <> errorCode <> " - " <> errorDesc
      updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
      throwError $ DigiLockerOperationFailed $ "Failed to verify and store driving license: " <> errorDesc

  return Success

verifySessionActive :: DDV.DigilockerVerification -> DVC.DocumentType -> Flow ()
verifySessionActive session docType = do
  now <- Kernel.Utils.Common.getCurrentTime
  let sessionAge = diffUTCTime now session.createdAt
  let oneHour = 3600 :: NominalDiffTime

  logInfo $ "PullDocument - Session age: " <> show sessionAge <> ", One hour: " <> show oneHour

  when (sessionAge > oneHour) $ do
    updateDocStatusField session.id docType "FAILED" (Just "PULL_DOC_FAILED") (Just "DigiLocker session expired")
    throwError DigiLockerSessionExpired

  unless (isJust session.accessToken) $ do
    updateDocStatusField session.id docType "FAILED" (Just "PULL_DOC_FAILED") (Just $ "DigiLocker session is not authorized. Current status: " <> show session.sessionStatus)
    throwError DigiLockerSessionUnauthorized

  logInfo $ "PullDocument - Verified session is active and within 1 hour"

verifyAndStoreDL ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString ->
  VerificationTypes.ExtractedDigiLockerDLResp ->
  Flow ()
verifyAndStoreDL session person pdfBytes extractedDL = do
  dlFlow <-
    extractedDL.extractedDL
      & fromMaybeM (InternalError "DL data not found in DigiLocker response")

  dlNumber <-
    dlFlow.dlNumber
      & fromMaybeM (InternalError "DL number not found in DigiLocker XML")

  dlExpiry <-
    dlFlow.expiryDate
      & fromMaybeM (InternalError "DL expiry not found in DigiLocker XML")

  logInfo $ "PullDocument - Extracted DL data: DL=" <> dlNumber <> ", Name=" <> show dlFlow.name <> ", Expiry=" <> show dlExpiry

  now <- Kernel.Utils.Common.getCurrentTime
  whenJust dlFlow.dob $ \dobText -> do
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobText) of
      Just dobUTC -> do
        let age = diffInYears dobUTC now
        when (age < 18 || age > 80) $
          throwError DigiLockerInvalidDriverAge
      Nothing -> logWarning $ "PullDocument - Could not parse DOB: " <> dobText

  mbExistingDL <- QDLE.findByDLNumber dlNumber
  whenJust mbExistingDL $ \existingDL -> do
    when (existingDL.driverId /= person.id) $
      throwError $ DocumentAlreadyLinkedToAnotherDriver "DL"
    when (existingDL.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "DL"
    when (existingDL.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "DL"

  logInfo $ "PullDocument - No blocking duplicate DL found, proceeding with upload"

  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.DriverLicense,
            rcNumber = Nothing,
            validationStatus = Just APITypes.AUTO_APPROVED,
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False Nothing Nothing (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "PullDocument - Uploaded DL PDF to S3 (DigiLocker verified), ImageId: " <> imageId.getId

  let vehicleCategory = session.vehicleCategory
  logInfo $ "PullDocument - Using vehicle category from session: " <> show vehicleCategory

  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory
      person.merchantOperatingCityId
      DVC.DriverLicense
      vehicleCategory
      Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DVC.DriverLicense <> " for category " <> show vehicleCategory))

  logInfo $ "PullDocument - Retrieved DocumentVerificationConfig for category: " <> show vehicleCategory

  let covDetails = case dlFlow.classOfVehicles of
        Just covs -> Just (createCovDetails covs)
        Nothing -> Nothing
  let dlExpiryText = dlExpiry
  let dobText = dlFlow.dob

  let dateOfIssueUTC =
        dlFlow.dateOfIssue >>= \dateText ->
          parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateText)

  logInfo $ "PullDocument - Calling onVerifyDLHandler with COVs: " <> show dlFlow.classOfVehicles <> ", VehicleCategory: " <> show vehicleCategory
  DLModule.onVerifyDLHandler
    person
    (Just dlNumber)
    (Just dlExpiryText)
    covDetails
    dlFlow.name
    dobText
    documentVerificationConfig
    imageId
    Nothing
    dlFlow.name
    dateOfIssueUTC
    (Just vehicleCategory)
  logInfo $ "PullDocument - Successfully stored DL via onVerifyDLHandler for DriverId: " <> person.id.getId

updateDocStatusField ::
  Id DDV.DigilockerVerification ->
  DVC.DocumentType ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Flow ()
updateDocStatusField sessionId docType statusText respCode respDesc = do
  mbSession <- QDV.findById sessionId
  whenJust mbSession $ \session -> do
    docStatusEnum <-
      case DocStatus.textToDocStatusEnum statusText of
        Just s -> pure s
        Nothing -> do
          logError $ "PullDocument - Failed to parse status: " <> statusText
          throwError $ InternalError $ "Invalid status: " <> statusText

    let currentDocStatusMap = session.docStatus
    let updatedDocStatusMap = DocStatus.updateDocStatus docType docStatusEnum respCode respDesc currentDocStatusMap
    QDV.updateDocStatus updatedDocStatusMap sessionId
    logInfo $ "PullDocument - Updated docStatus for " <> show docType <> " to " <> statusText

base64Encode :: BS.ByteString -> Text
base64Encode = TE.decodeUtf8 . B64.encode

diffInYears :: Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Int
diffInYears t1 t2 =
  let days1 = utctDay t1
      days2 = utctDay t2
      daysDiff = abs $ diffDays days2 days1
   in fromIntegral daysDiff `div` 365

createCovDetails :: [Text] -> [Idfy.CovDetail]
createCovDetails = map createCovDetail
  where
    createCovDetail :: Text -> Idfy.CovDetail
    createCovDetail covText =
      Idfy.CovDetail
        { Idfy.category = Nothing,
          Idfy.cov = covText,
          Idfy.issue_date = Nothing
        }

extractDigiLockerError :: DigiLockerError.DigiLockerError -> (Text, Text)
extractDigiLockerError err = case err of
  DigiLockerError.DGLUnauthorizedError ->
    ("DGL_UNAUTHORIZED", "DigiLocker token expired or has been revoked by user")
  DigiLockerError.DGLBadRequestError msg ->
    ("DGL_BAD_REQUEST", msg)
  DigiLockerError.DGLForbiddenError msg ->
    ("DGL_FORBIDDEN", "Insufficient scope: " <> msg)
  DigiLockerError.DGLNotFoundError msg ->
    ("DGL_NOT_FOUND", "Document not found: " <> msg)
  DigiLockerError.DGLInternalServerError msg ->
    ("DGL_INTERNAL_SERVER_ERROR", "DigiLocker service error: " <> msg)
  DigiLockerError.DGLError msg ->
    ("DGL_ERROR", msg)
