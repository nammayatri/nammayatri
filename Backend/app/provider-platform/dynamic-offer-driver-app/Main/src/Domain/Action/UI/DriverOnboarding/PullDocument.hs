{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.PullDocument
  ( pullDrivingLicenseDocument,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, diffDays, parseTimeM, utctDay)
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DLModule
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
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

-- | Pull driving license document from DigiLocker
-- This endpoint is called when user submits DL details within 1 hour of starting DigiLocker session
pullDrivingLicenseDocument ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  APITypes.PullDrivingLicenseReq ->
  Flow APISuccess
pullDrivingLicenseDocument (mbDriverId, merchantId, merchantOpCityId) req = do
  -- Step 1: Extract driverId from auth token
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "PullDocument - Starting pull for DriverId: " <> driverId.getId <> ", DocType: " <> show req.docType

  -- Step 2: Fetch person details
  person <- PersonQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- Step 3: Check if DigiLocker is enabled in transporter config
  transporterConfig <-
    CQTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $ do
    logError $ "DigiLocker pull document - DigiLocker not enabled for merchantOpCityId: " <> person.merchantOperatingCityId.getId
    throwError $ InvalidRequest "DigiLocker verification is not enabled for this merchant operating city"

  -- Step 4: Fetch latest DigiLocker session
  latestSession <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  session <- case latestSession of
    [] -> throwError $ InvalidRequest "No active DigiLocker session found. Please initiate DigiLocker first."
    (s : _) -> return s

  -- Step 5: Verify document type is Driving License
  unless (req.docType == DVC.DriverLicense) $
    throwError $ InvalidRequest "Only Driving License documents are supported for pull operation"

  -- Step 6: Verify session is active and within 1 hour
  verifySessionActive session req.docType

  -- Step 7: Get access token from session
  accessToken <- session.accessToken & fromMaybeM (InvalidRequest "DigiLocker session not authorized. Access token missing.")

  -- Step 8: Get DigiLocker config
  digiLockerConfig <- DigilockerLockerShared.getDigiLockerConfig person.merchantOperatingCityId

  let orgId = digiLockerConfig.dlOrgId
  -- Step 9: Construct pull request with orgid from config
  let pullReq =
        DigiTypes.DigiLockerPullDrivingLicenseReq
          { accessToken = accessToken,
            orgid = orgId,
            doctype = "DRVLC", -- Driving License document type
            consent = "Y", -- Always Y as per requirement
            dlno = req.drivingLicenseNumber
          }

  -- Step 10: Call DigiLocker API to pull the document (with error handling)
  mbPullResp <-
    ( do
        logInfo $ "PullDocument - Calling DigiLocker pull API"
        pullResp <- Verification.pullDigiLockerDrivingLicense merchantId merchantOpCityId pullReq
        logInfo $ "PullDocument - Successfully pulled DL document, URI: " <> pullResp.uri
        return $ Just pullResp
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to pull DL: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ InvalidRequest $ "Failed to pull driving license from DigiLocker: " <> errorDesc

  pullResp <- mbPullResp & fromMaybeM (InternalError "Pull response is Nothing")

  -- Step 11: Fetch and extract DL data from DigiLocker (with error handling)
  mbExtractedResp <-
    ( do
        logInfo $ "PullDocument - Fetching and extracting DL data"
        let extractReq =
              DigiTypes.DigiLockerExtractDLReq
                { accessToken = accessToken,
                  uri = pullResp.uri
                }
        extractedResp <- Verification.fetchAndExtractVerifiedDL merchantId merchantOpCityId extractReq
        logInfo $ "PullDocument - Successfully extracted DL data"
        return $ Just extractedResp
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to extract DL: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ InvalidRequest $ "Failed to extract driving license data: " <> errorDesc

  extractedResp <- mbExtractedResp & fromMaybeM (InternalError "Extracted response is Nothing")

  -- Step 12: Get PDF file from DigiLocker (with error handling)
  mbPdfBytes <-
    ( do
        logInfo $ "PullDocument - Fetching PDF from DigiLocker"
        let fileReq =
              DigiTypes.DigiLockerGetFileReq
                { accessToken = accessToken,
                  uri = pullResp.uri
                }
        pdfBytes <- Verification.getDigiLockerFile merchantId merchantOpCityId fileReq
        logInfo $ "PullDocument - Successfully fetched PDF"
        return $ Just pdfBytes
      )
      `catch` \(err :: DigiLockerError.DigiLockerError) -> do
        let (errorCode, errorDesc) = extractDigiLockerError err
        logError $ "PullDocument - Failed to fetch PDF: " <> errorCode <> " - " <> errorDesc
        updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
        throwError $ InvalidRequest $ "Failed to fetch driving license PDF: " <> errorDesc

  pdfBytes <- mbPdfBytes & fromMaybeM (InternalError "PDF bytes is Nothing")

  -- Step 13: Verify and store DL data in database (with error handling)
  ( do
      logInfo $ "PullDocument - Verifying and storing DL data in database"
      verifyAndStoreDL session person pdfBytes extractedResp
      logInfo $ "PullDocument - Successfully stored DL in database"

      -- Step 14: Update docStatus in session to SUCCESS
      updateDocStatusField session.id req.docType "DOC_SUCCESS" (Just "200") (Just "Driver License verified and stored successfully via pull")
      logInfo $ "PullDocument - Successfully completed pull operation for DriverId: " <> driverId.getId
    )
    `catch` \(err :: DigiLockerError.DigiLockerError) -> do
      let (errorCode, errorDesc) = extractDigiLockerError err
      logError $ "PullDocument - Verification failed: " <> errorCode <> " - " <> errorDesc
      updateDocStatusField session.id req.docType "FAILED" (Just errorCode) (Just errorDesc)
      throwError $ InvalidRequest $ "Failed to verify and store driving license: " <> errorDesc

  return Success

-- | Verify session is active and within 1 hour of creation
verifySessionActive :: DDV.DigilockerVerification -> DVC.DocumentType -> Flow ()
verifySessionActive session docType = do
  now <- Kernel.Utils.Common.getCurrentTime
  let sessionAge = diffUTCTime now session.createdAt
  let oneHour = 3600 :: NominalDiffTime

  logInfo $ "PullDocument - Session age: " <> show sessionAge <> ", One hour: " <> show oneHour

  -- Check if session is within 1 hour
  when (sessionAge > oneHour) $ do
    updateDocStatusField session.id docType "FAILED" (Just "PULL_DOC_FAILED") (Just "DigiLocker session expired")
    throwError $ InvalidRequest "DigiLocker session has expired. Please initiate a new session."

  -- Check if session has access token (which means it's authorized)
  unless (isJust session.accessToken) $ do
    updateDocStatusField session.id docType "FAILED" (Just "PULL_DOC_FAILED") (Just $ "DigiLocker session is not authorized. Current status: " <> show session.sessionStatus)
    throwError $ InvalidRequest $ "DigiLocker session is not authorized. Current status: " <> show session.sessionStatus

  logInfo $ "PullDocument - Verified session is active and within 1 hour"

-- | Verify and store Driver License data
-- Extracts data from DigiLocker, uploads PDF to S3, and calls existing DL registration logic
verifyAndStoreDL ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString -> -- PDF data
  VerificationTypes.ExtractedDigiLockerDLResp -> -- Extracted DL response
  Flow ()
verifyAndStoreDL session person pdfBytes extractedDL = do
  -- Step 1: Extract DL data from DigiLocker response
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

  -- Step 2: Validate age (18-80 years) if DOB is present
  now <- Kernel.Utils.Common.getCurrentTime
  whenJust dlFlow.dob $ \dobText -> do
    -- Parse DOB text to UTCTime (format: DD-MM-YYYY from DigiLocker)
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobText) of
      Just dobUTC -> do
        let age = diffInYears dobUTC now
        when (age < 18 || age > 80) $
          throwError $ InvalidRequest $ "Driver age must be between 18 and 80 years. Current age: " <> show age
      Nothing -> logWarning $ "PullDocument - Could not parse DOB: " <> dobText

  -- Step 3: Check for duplicate DL BEFORE uploading to S3 (avoid waste)
  mbExistingDL <- QDLE.findByDLNumber dlNumber
  whenJust mbExistingDL $ \existingDL -> do
    -- Check if DL is linked to another driver
    when (existingDL.driverId /= person.id) $
      throwError $ DocumentAlreadyLinkedToAnotherDriver "DL"
    -- Check if DL is under manual review
    when (existingDL.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "DL"
    -- Check if DL is already validated
    when (existingDL.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "DL"

  logInfo $ "PullDocument - No blocking duplicate DL found, proceeding with upload"

  -- Step 4: Upload PDF to S3 via validateImage
  -- DigiLocker documents are government-verified, so we mark them as AUTO_APPROVED
  -- to skip external Idfy verification
  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.DriverLicense,
            rcNumber = Nothing,
            validationStatus = Just APITypes.AUTO_APPROVED, -- Skip Idfy verification for DigiLocker docs
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "PullDocument - Uploaded DL PDF to S3 (DigiLocker verified), ImageId: " <> imageId.getId

  -- Step 5: Get vehicle category from session (driver selected this during /initiate)
  let vehicleCategory = session.vehicleCategory
  logInfo $ "PullDocument - Using vehicle category from session: " <> show vehicleCategory

  -- Step 6: Get DocumentVerificationConfig for validation
  -- This config contains the supported COVs for the selected vehicle category
  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory
      person.merchantOperatingCityId
      DVC.DriverLicense
      vehicleCategory
      Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DVC.DriverLicense <> " for category " <> show vehicleCategory))

  logInfo $ "PullDocument - Retrieved DocumentVerificationConfig for category: " <> show vehicleCategory

  -- Step 7: Convert extracted data to format expected by onVerifyDLHandler
  -- Convert COVs from [Text] to [Idfy.CovDetail]
  -- classOfVehicles is Maybe [Text], so we need to handle the Maybe
  let covDetails = case dlFlow.classOfVehicles of
        Just covs -> Just (createCovDetails covs)
        Nothing -> Nothing
  -- DigiLocker returns all dates as Text in DD-MM-YYYY format, no conversion needed for display
  let dlExpiryText = dlExpiry
  let dobText = dlFlow.dob

  -- Parse dateOfIssue from Text to UTCTime (onVerifyDLHandler expects Maybe UTCTime)
  let dateOfIssueUTC =
        dlFlow.dateOfIssue >>= \dateText ->
          parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateText)

  logInfo $ "PullDocument - Calling onVerifyDLHandler with COVs: " <> show dlFlow.classOfVehicles <> ", VehicleCategory: " <> show vehicleCategory

  -- Step 8: Call onVerifyDLHandler to validate and store DL
  -- This will validate that DL's COVs match the selected vehicle category
  DLModule.onVerifyDLHandler
    person
    (Just dlNumber)
    (Just dlExpiryText)
    covDetails
    dlFlow.name
    dobText
    documentVerificationConfig
    imageId
    Nothing -- imageId2
    dlFlow.name -- nameOnCard
    dateOfIssueUTC -- dateOfIssue parsed to UTCTime
    (Just vehicleCategory) -- vehicleCategory from session (driver's selection)
  logInfo $ "PullDocument - Successfully stored DL via onVerifyDLHandler for DriverId: " <> person.id.getId

-- | Update a single document's status in docStatus JSON
updateDocStatusField ::
  Id DDV.DigilockerVerification ->
  DVC.DocumentType ->
  Text -> -- status
  Maybe Text -> -- response code
  Maybe Text -> -- response description
  Flow ()
updateDocStatusField sessionId docType status respCode respDesc = do
  -- Fetch current session
  mbSession <- QDV.findById sessionId
  whenJust mbSession $ \session -> do
    -- Parse current docStatus JSON
    let currentDocStatus = session.docStatus
    case A.fromJSON currentDocStatus of
      A.Success (docMap :: HM.HashMap Text A.Value) -> do
        -- Update the specific document status
        let docKey = docTypeToText docType
        let updatedDocObj =
              A.object
                [ "status" A..= status,
                  "responseCode" A..= respCode,
                  "responseDescription" A..= respDesc
                ]
        let updatedMap = HM.insert docKey updatedDocObj docMap
        let updatedJson = A.toJSON updatedMap

        -- Update in database
        QDV.updateDocStatus updatedJson sessionId
        logInfo $ "PullDocument - Updated docStatus for " <> docKey <> " to " <> status
      A.Error err -> do
        logError $ "PullDocument - Failed to parse docStatus JSON: " <> T.pack err

-- Helper functions
base64Encode :: BS.ByteString -> Text
base64Encode = TE.decodeUtf8 . B64.encode

diffInYears :: Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Int
diffInYears t1 t2 =
  let days1 = utctDay t1
      days2 = utctDay t2
      daysDiff = abs $ diffDays days2 days1
   in fromIntegral daysDiff `div` 365

-- | Convert list of COV texts to Idfy.CovDetail format
-- DigiLocker provides COVs as [Text], but onVerifyDLHandler expects [Idfy.CovDetail]
createCovDetails :: [Text] -> [Idfy.CovDetail]
createCovDetails covs = map createCovDetail covs
  where
    createCovDetail :: Text -> Idfy.CovDetail
    createCovDetail covText =
      Idfy.CovDetail
        { Idfy.category = Nothing, -- DigiLocker doesn't provide COV category
          Idfy.cov = covText, -- The actual COV text (e.g., "MCWG", "LMV")
          Idfy.issue_date = Nothing -- DigiLocker doesn't provide per-COV issue dates
        }

docTypeToText :: DVC.DocumentType -> Text
docTypeToText = T.pack . show

-- | Extract error code and description from DigiLocker error
-- Maps DigiLockerError to (errorCode, errorDescription) tuple for storage in DB
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
