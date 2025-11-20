module Domain.Action.UI.DriverOnboarding.DigiLockerCallback where

import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), utctDay)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DLModule
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboardingV2 as DriverOnboardingV2
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverPanCard as DPC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as VehicleCategory
import Environment
import Kernel.External.Encryption (encrypt)
import qualified Kernel.External.SharedLogic.DigiLocker.Error as DigiLockerError
import qualified Kernel.External.Tokenize.Digilocker.Types as TokenizeTypes
import qualified Kernel.External.Tokenize.Interface as Tokenize
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.Digilocker.Types as DigilockerTypes
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.Types as VerificationTypes
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import qualified SharedLogic.DriverOnboarding.Digilocker as SDDigilocker
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.Queries.AadhaarCard as QAC
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverLicenseExtra as QDLE
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Verification as Verification

-- | DigiLocker OAuth 2.0 Callback API
-- This endpoint receives the OAuth callback from DigiLocker after user authorization.
--
-- Flow:
-- 1. App initiates OAuth flow with response_type=code
-- 2. User authorizes on DigiLocker
-- 3. DigiLocker redirects to this callback with authorization code
-- 4. We exchange the code for an access token (via tokenization API)
-- 5. Use access token to pull documents from DigiLocker
--
-- Success Parameters (response_type=code):
--   - code: Authorization code to exchange for access token
--   - state: Application-specific data (contains driverId and codeVerifier)
--
-- Error Parameters (OAuth 2.0 spec):
--   - error: Error code as per OAuth 2.0 spec
--   - error_description: Human-readable error description
--   - state: Original state (if available)
--
-- NOTE: Either 'code' OR 'error' will be present, not both
type DigiLockerCallbackAPI =
  "dobpp"
    :> "verify"
    :> "callback"
    :> "digiLocker"
    :> QueryParam "error" Text
    :> QueryParam "error_description" Text
    :> QueryParam "code" Text
    :> MandatoryQueryParam "state" Text
    :> Get '[JSON] AckResponse

-- | Handler for DigiLocker OAuth 2.0 callback
-- This is called by DigiLocker after user authorization
-- Parameters follow OAuth 2.0 Authorization Code flow
digiLockerCallbackHandler ::
  Maybe Text -> -- error (OAuth 2.0 error code)
  Maybe Text -> -- error_description
  Maybe Text -> -- code (authorization code) - only present in success case
  Text -> -- state (contains stateID)
  Flow AckResponse
digiLockerCallbackHandler mbError mbErrorDescription mbCode stateParam = do
  logInfo $ "DigiLocker OAuth Callback Received - State: " <> stateParam

  -- Step 1: Validate state parameter (if empty, we can't identify driver - set alert)
  when (T.null stateParam) $ do
    logError "DigiLocker callback - Missing required state parameter. Cannot identify driver."
    throwError $ InternalError "DigiLocker callback received with empty state parameter"

  -- Step 2: Find session by stateId once at the beginning (for driver ID logging and later use)
  -- We fetch it early so we can log driver ID in error cases without duplicate DB calls
  mbSession <- QDV.findByStateId stateParam
  let driverIdStr = case mbSession of
        Just session -> ", DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId
        Nothing -> ", StateId: " <> stateParam <> " (driver not found)"

  -- Step 3: Handle OAuth error cases (e.g., access_denied)
  whenJust mbError $ \errorCode -> do
    let errorMsg = fromMaybe "Unknown OAuth error" mbErrorDescription
    logError $ "DigiLocker OAuth Error - Code: " <> errorCode <> ", Description: " <> errorMsg <> driverIdStr

    -- Update session status to CONSENT_DENIED if error is access_denied
    when (errorCode == "access_denied") $ do
      whenJust mbSession $ \session -> do
        QDV.updateSessionStatus DDV.CONSENT_DENIED (Just errorCode) (Just errorMsg) session.id
        logInfo $ "DigiLocker callback - Updated session status to CONSENT_DENIED for DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId

    throwError $ InvalidRequest $ "DigiLocker OAuth Error: " <> errorCode <> " - " <> errorMsg

  -- Step 4: Validate authorization code
  code <- mbCode & fromMaybeM (InvalidRequest "DigiLocker callback - Missing authorization code")
  when (T.null code) $ do
    logError $ "DigiLocker callback - Authorization code is empty" <> driverIdStr
    throwError $ InvalidRequest "DigiLocker callback received with empty authorization code"

  -- Step 5: Ensure we have a valid session (already fetched above, but validate here)
  session <- mbSession & fromMaybeM (InvalidRequest "Invalid or expired state parameter from DigiLocker")
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker callback - Found session for DriverId: " <> driverId.getId <> ", StateId: " <> stateId

  -- Step 6: Get person and validate DigiLocker is enabled for this merchant operating city
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- Check if DigiLocker is enabled in transporter config
  transporterConfig <-
    CQTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $ do
    logError $ "DigiLocker callback - DigiLocker not enabled for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", merchantOpCityId: " <> person.merchantOperatingCityId.getId
    throwError $ InvalidRequest "DigiLocker verification is not enabled for this merchant operating city"

  -- Step 7: Get DigiLocker config and call tokenize API
  digiLockerConfig <- SDDigilocker.getDigiLockerConfig person.merchantOperatingCityId
  logInfo $
    "DigiLocker callback - Loaded DigiLocker config for DriverId: "
      <> driverId.getId
      <> ", StateId: "
      <> stateId
      <> ", merchantOpCityId: "
      <> person.merchantOperatingCityId.getId
      <> ", baseUrl: "
      <> show digiLockerConfig.url

  let tokenReq =
        InterfaceTypes.TokenizationReq
          { code = Just code,
            codeVerifier = Just session.codeVerifier,
            expiry = Nothing
          }

  -- Convert DigiLockerCfg to DigilockerTokenizeConfig (tokenize needs encrypted secret)
  encryptedSecret <- encrypt digiLockerConfig.clientSecret
  let tokenizeConfig =
        TokenizeTypes.DigilockerTokenizeConfig
          { url = digiLockerConfig.url,
            clientId = digiLockerConfig.clientId,
            clientSecret = encryptedSecret,
            redirectUri = digiLockerConfig.redirectUri
          }

  logInfo $
    "DigiLocker callback - Calling Tokenize.tokenize for DriverId: "
      <> driverId.getId
      <> ", StateId: "
      <> stateId
      <> ", codeVerifier present: "
      <> show (isJust tokenReq.codeVerifier)
      <> ", state: present"
  tokenResp <-
    Tokenize.tokenize (InterfaceTypes.DigilockerTokenizationServiceConfig tokenizeConfig) tokenReq
      `catchAny` \err -> do
        logError $ "DigiLocker callback - Token API failed for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ". Error: " <> show err
        QDV.updateSessionStatus DDV.FAILED (Just "TOKEN_API_FAILED") (Just $ T.pack $ show err) session.id
        throwError $ InternalError "Failed to obtain access token from DigiLocker"

  logInfo $
    "DigiLocker callback - Token API success for DriverId: "
      <> driverId.getId
      <> ", StateId: "
      <> stateId
      <> ". Access token obtained, scope: "
      <> show tokenResp.scope

  -- Step 8: Update session with access token, scope, and expiry
  let accessToken = tokenResp.token
  let scope = tokenResp.scope
  let expiresAt = tokenResp.expiresAt

  QDV.updateAccessToken (Just accessToken) expiresAt (Just code) scope stateParam

  -- Step 9: Get required documents and already verified documents
  requiredDocs <- getRequiredDocuments person.merchantOperatingCityId session.vehicleCategory
  alreadyVerifiedDocs <- getAlreadyVerifiedDocuments driverId
  let unavailableDocs = filter (`notElem` alreadyVerifiedDocs) (map (.documentType) requiredDocs)

  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Required docs: " <> show (map (.documentType) requiredDocs)
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Already verified: " <> show alreadyVerifiedDocs
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Unavailable docs: " <> show unavailableDocs

  -- Step 10: Parse scope to identify issued and pull documents
  let scopeText = fromMaybe "" scope
  let parsedDocs = parseDigiLockerScope scopeText

  -- Step 11: Check consent for unavailable docs
  let (consentGranted, consentDenied) = checkDocumentConsent unavailableDocs parsedDocs

  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Consent granted for: " <> show consentGranted
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Consent denied for: " <> show consentDenied

  -- Step 12: Check if any required doc has no consent - if so, mark session as CONSENT_DENIED
  -- Don't initialize docStatus since we won't process any documents
  if not (null consentDenied)
    then do
      QDV.updateSessionStatus DDV.CONSENT_DENIED Nothing (Just $ "Missing consent for: " <> T.intercalate ", " (map show consentDenied)) session.id
      logWarning $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Missing consent for required documents: " <> show consentDenied
      logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Scope stored for debugging. Parse scope to see granted vs denied docs."
      return Ack
    else do
      -- Step 13: All consent granted - Initialize docStatus JSON with individual doc statuses
      let initialDocStatus = initializeDocStatus unavailableDocs parsedDocs
      QDV.updateDocStatus initialDocStatus session.id
      logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Initialized docStatus for " <> show (length unavailableDocs) <> " documents"

      -- Step 14: Mark session as SUCCESS (token obtained and consent given for all required docs)
      QDV.updateSessionStatus DDV.SUCCESS Nothing Nothing session.id

      -- Step 15: Process documents in background (for issued docs only, pull docs require user input)
      fork "digilocker-verify-documents" $ do
        processDocuments session person digiLockerConfig accessToken parsedDocs unavailableDocs
          `catchAny` \err -> do
            logError $ "DigiLocker - Catastrophic failure in background job - Error: " <> show err <> ", DriverId: " <> driverId.getId <> ", StateId: " <> stateId
            -- Update session status to FAILED (individual doc errors are already handled)
            -- Don't update docStatus - DB might be broken in catastrophic failures
            QDV.updateSessionStatus DDV.FAILED (Just "FORK_FAILED") (Just $ T.pack $ show err) session.id

      logInfo $ "DigiLocker callback - Successfully completed callback processing for DriverId: " <> driverId.getId <> ", StateId: " <> stateId
      return Ack

----------- Helper Functions -----------

-- | Get DigiLocker config from merchant service config
-- Assumes digilockerEnabled check is already done by caller
-- TODO: Implement proper config retrieval from MerchantServiceConfig table
-- getDigiLockerConfig :: Id DMOC.MerchantOperatingCity -> Flow DigilockerTypes.DigiLockerCfg
-- getDigiLockerConfig _merchantOpCityId = do
--   -- TODO: Fetch DigiLocker credentials from MerchantServiceConfig
--   -- The config should contain: clientId, clientSecret, redirectUri, url
--   -- For now, throwing error as this integration needs to be added
--   throwError $ InternalError "DigiLocker service config retrieval not yet implemented. Please configure DigiLocker in merchant_service_config table."

-- | Get list of required (mandatory) documents for a merchant operating city
getRequiredDocuments ::
  Id DMOC.MerchantOperatingCity ->
  VehicleCategory.VehicleCategory ->
  Flow [DVC.DocumentVerificationConfig]
getRequiredDocuments merchantOpCityId vehicleCategory = do
  docsForCategory <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId vehicleCategory Nothing
  let digiLockerSupportedDocs =
        [ DVC.PanCard,
          DVC.AadhaarCard,
          DVC.DriverLicense
        ]
  return $
    docsForCategory
      & filter (.isMandatory)
      & filter
        ( (\config -> config.documentType `elem` digiLockerSupportedDocs)
        )

-- | Get list of already verified documents for a driver
-- Only returns documents that exist AND have verificationStatus == VALID
-- This matches the logic used in statusHandler
getAlreadyVerifiedDocuments :: Id DP.Person -> Flow [DVC.DocumentType]
getAlreadyVerifiedDocuments driverId = do
  verifiedDocs <- catMaybes <$> sequence [checkPAN, checkAadhaar, checkDL]
  return verifiedDocs
  where
    checkPAN = do
      mbPan <- QDPC.findByDriverId driverId
      return $ case mbPan of
        Just pan | pan.verificationStatus == Documents.VALID -> Just DVC.PanCard
        _ -> Nothing

    checkAadhaar = do
      mbAadhaar <- QAC.findByPrimaryKey driverId
      return $ case mbAadhaar of
        Just aadhaar | aadhaar.verificationStatus == Documents.VALID -> Just DVC.AadhaarCard
        _ -> Nothing

    checkDL = do
      mbDL <- QDL.findByDriverId driverId
      return $ case mbDL of
        Just dl | dl.verificationStatus == Documents.VALID -> Just DVC.DriverLicense
        _ -> Nothing

-- | Data type to represent document availability in DigiLocker scope
data DocumentAvailability
  = Issued Text -- URI for issued document
  | PullRequired -- Document needs to be pulled with user input
  | ConsentDenied -- User did not give consent
  deriving (Show, Eq)

-- | Type to represent extracted document data from DigiLocker
data ExtractedDocumentData
  = ExtractedPan VerificationTypes.ExtractedDigiLockerPanResp
  | ExtractedAadhaar VerificationTypes.ExtractedDigiLockerAadhaarResp
  | ExtractedDL VerificationTypes.ExtractedDigiLockerDLResp
  deriving (Show)

-- | Parse DigiLocker scope to extract document URIs
-- Examples:
--   - Pull required: "picture userdetails file.partners/DRVLC files.issueddocs"
--   - Issued: "picture files.issueddocs issued/in.gov.transport-DRVLC-DL0920070347594 userdetails"
parseDigiLockerScope :: Text -> [(DVC.DocumentType, DocumentAvailability)]
parseDigiLockerScope scopeText =
  let tokens = T.words scopeText
      parsedDocs = mapMaybe parseToken tokens
   in parsedDocs
  where
    parseToken :: Text -> Maybe (DVC.DocumentType, DocumentAvailability)
    parseToken token
      -- Issued document with URI: issued/in.gov.pan-PANCR-ABCDE1234F
      | "issued/" `T.isPrefixOf` token = do
        docType <- extractDocType token
        let uri = fromMaybe token (T.stripPrefix "issued/" token)
        return (docType, Issued uri)
      -- Pull required: file.partners/DRVLC
      | "file.partners/" `T.isPrefixOf` token = do
        docType <- extractDocTypeFromPartners token
        return (docType, PullRequired)
      | otherwise = Nothing

    extractDocType :: Text -> Maybe DVC.DocumentType
    extractDocType uri
      | "PANCR" `T.isInfixOf` tokenUpper = Just DVC.PanCard
      | any (`T.isInfixOf` tokenUpper) ["AADHAAR", "ADHAR"] = Just DVC.AadhaarCard
      | "DRVLC" `T.isInfixOf` tokenUpper = Just DVC.DriverLicense
      | otherwise = Nothing
      where
        tokenUpper = T.toUpper uri

    extractDocTypeFromPartners :: Text -> Maybe DVC.DocumentType
    extractDocTypeFromPartners token
      | "PANCR" `T.isInfixOf` tokenUpper = Just DVC.PanCard
      | "DRVLC" `T.isInfixOf` tokenUpper = Just DVC.DriverLicense
      | any (`T.isInfixOf` tokenUpper) ["AADHAAR", "ADHAR"] = Just DVC.AadhaarCard
      | otherwise = Nothing
      where
        tokenUpper = T.toUpper token

-- | Check which required documents have consent and which don't
checkDocumentConsent ::
  [DVC.DocumentType] ->
  [(DVC.DocumentType, DocumentAvailability)] ->
  ([DVC.DocumentType], [DVC.DocumentType]) -- (granted, denied)
checkDocumentConsent requiredDocs parsedDocs =
  let availableDocs = map fst parsedDocs
      granted = filter (`elem` availableDocs) requiredDocs
      denied = filter (`notElem` availableDocs) requiredDocs
   in (granted, denied)

-- | Initialize DocStatusMap with document statuses
-- Uses strongly-typed DocumentType as keys
initializeDocStatus ::
  [DVC.DocumentType] ->
  [(DVC.DocumentType, DocumentAvailability)] ->
  DocStatus.DocStatusMap
initializeDocStatus unavailableDocs parsedDocs =
  foldl' updateDocStatus DocStatus.emptyDocStatusMap unavailableDocs
  where
    updateDocStatus acc docType =
      let availability = getAvailability docType
          docStatusEnum = case availability of
            ConsentDenied -> DocStatus.DOC_CONSENT_DENIED
            PullRequired -> DocStatus.DOC_PULL_REQUIRED
            Issued _ -> DocStatus.DOC_PENDING
       in DocStatus.updateDocStatus docType docStatusEnum Nothing Nothing acc

    getAvailability :: DVC.DocumentType -> DocumentAvailability
    getAvailability docType =
      case find (\(dt, _) -> dt == docType) parsedDocs of
        Just (_, avail) -> avail
        Nothing -> ConsentDenied

-- | Process all documents - call APIs and verify (background job)
-- Each document is processed independently with try-catch to ensure one failure doesn't affect others
processDocuments ::
  DDV.DigilockerVerification ->
  DP.Person ->
  DigilockerTypes.DigiLockerCfg ->
  Text -> -- access token
  [(DVC.DocumentType, DocumentAvailability)] -> -- parsed docs
  [DVC.DocumentType] -> -- unavailable docs to process
  Flow ()
processDocuments session person _digiLockerConfig accessToken parsedDocs unavailableDocs = do
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Starting document processing for " <> show (length unavailableDocs) <> " documents"

  -- Process each unavailable document type independently with error handling
  forM_ unavailableDocs $ \docType -> do
    -- Wrap each document processing in try-catch to ensure failures are independent
    ( do
        case find (\(dt, _) -> dt == docType) parsedDocs of
          Nothing -> do
            logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", No consent found for docType: " <> show docType
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_CONSENT_DENIED) (Just "NO_CONSENT") (Just "User did not grant consent for this document")
          Just (_, availability) -> do
            case availability of
              PullRequired -> do
                -- Mark as PULL_REQUIRED - user needs to provide document details
                updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_PULL_REQUIRED) Nothing Nothing
                logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Marked " <> show docType <> " as PULL_REQUIRED (user needs to enter details)"
              ConsentDenied -> do
                -- Mark as CONSENT_DENIED
                updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_CONSENT_DENIED) (Just "NO_CONSENT") (Just "User denied consent")
                logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Marked " <> show docType <> " as CONSENT_DENIED"
              Issued uri -> do
                -- Process issued document - fetch and verify
                processIssuedDocument session person accessToken docType uri
      )
      `catchAny` \err -> do
        -- If processing this document fails, log and mark as failed, but continue with other documents
        logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to process " <> show docType <> ": " <> show err
        updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just "PROCESSING_ERROR") (Just $ T.pack $ show err)

  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Completed processing all documents"

-- | Process a single issued document - fetch file (PDF), extract data (XML), and verify
-- This function has internal error handling to ensure each API call is tried independently
processIssuedDocument ::
  DDV.DigilockerVerification ->
  DP.Person ->
  Text -> -- access token
  DVC.DocumentType ->
  Text -> -- URI
  Flow ()
processIssuedDocument session person accessToken docType uri = do
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Processing issued document: " <> show docType <> ", URI: " <> uri

  -- Step 1: Fetch PDF file for S3 storage (with error handling)
  -- Note: Aadhaar doesn't support getFile API, so we skip PDF fetch for it
  mbPdfData <-
    if docType == DVC.AadhaarCard
      then do
        logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Skipping PDF fetch for Aadhaar (not supported by DigiLocker)"
        return Nothing
      else
        ( do
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching PDF for " <> show docType
            let fileReq =
                  DigiTypes.DigiLockerGetFileReq
                    { accessToken = accessToken,
                      uri = uri
                    }
            logInfo $
              "DigiLocker - DriverId: "
                <> driverId.getId
                <> ", StateId: "
                <> stateId
                <> ", Calling getDigiLockerFile with uri: "
                <> uri
                <> ", merchantOpCityId: "
                <> person.merchantOperatingCityId.getId
            logInfo $
              "DigiLocker - DriverId: "
                <> driverId.getId
                <> ", StateId: "
                <> stateId
                <> ", getDigiLockerFile input: merchantId="
                <> person.merchantId.getId
                <> ", merchantOpCityId="
                <> person.merchantOperatingCityId.getId
                <> ", uri="
                <> uri
            pdfBytes <- Verification.getDigiLockerFile person.merchantId person.merchantOperatingCityId fileReq
            logInfo $
              "DigiLocker - DriverId: "
                <> driverId.getId
                <> ", StateId: "
                <> stateId
                <> ", getDigiLockerFile output bytes: "
                <> show (BSL.length pdfBytes)
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully fetched PDF for " <> show docType
            return $ Just pdfBytes
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            -- Handle DigiLocker-specific errors with proper error codes
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to fetch PDF for " <> show docType <> ": " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing

  -- Step 2: Fetch raw XML for Aadhaar (for S3 storage)
  -- Aadhaar doesn't support getFile (PDF), so we fetch raw XML separately
  mbAadhaarXml <-
    if docType == DVC.AadhaarCard
      then
        ( do
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching raw Aadhaar XML for S3 storage"
            let extractReq =
                  DigiTypes.DigiLockerExtractAadhaarReq
                    { accessToken = accessToken
                    }
            logInfo $
              "DigiLocker - DriverId: "
                <> driverId.getId
                <> ", StateId: "
                <> stateId
                <> ", Calling getVerifiedAadhaarXML for merchantOpCityId: "
                <> person.merchantOperatingCityId.getId
            xmlText <- Verification.getVerifiedAadhaarXML person.merchantId person.merchantOperatingCityId extractReq
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully fetched Aadhaar XML"
            return $ Just xmlText
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to fetch Aadhaar XML: " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing
      else return Nothing

  -- Step 3: Fetch and extract/parse XML data (with error handling)
  -- For Aadhaar: Fetch parsed data (XML already fetched above)
  -- For PAN/DL: Only proceed if PDF fetch succeeded
  mbExtractedData <- case (mbPdfData, docType) of
    (Nothing, DVC.AadhaarCard)
      | isJust mbAadhaarXml ->
        ( do
            -- Aadhaar: XML already fetched, now get parsed data
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Extracting Aadhaar data from XML"
            let extractReq =
                  DigiTypes.DigiLockerExtractAadhaarReq
                    { accessToken = accessToken
                    }
            logInfo $
              "DigiLocker - DriverId: "
                <> driverId.getId
                <> ", StateId: "
                <> stateId
                <> ", Calling fetchAndExtractVerifiedAadhaar for merchantOpCityId: "
                <> person.merchantOperatingCityId.getId
            extractedResp <- Verification.fetchAndExtractVerifiedAadhaar person.merchantId person.merchantOperatingCityId extractReq
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully extracted Aadhaar data"
            return $ Just (ExtractedAadhaar extractedResp)
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to extract Aadhaar: " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing
    (Nothing, _) -> return Nothing -- PDF/XML fetch failed for non-Aadhaar docs, skip extraction
    (Just _pdfBytes, DVC.PanCard) ->
      ( do
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching and extracting PAN data"
          let extractReq =
                DigiTypes.DigiLockerExtractPanReq
                  { accessToken = accessToken,
                    uri = uri
                  }
          logInfo $
            "DigiLocker - DriverId: "
              <> driverId.getId
              <> ", StateId: "
              <> stateId
              <> ", Calling fetchAndExtractVerifiedPan with uri: "
              <> uri
              <> ", merchantOpCityId: "
              <> person.merchantOperatingCityId.getId
          extractedResp <- Verification.fetchAndExtractVerifiedPan person.merchantId person.merchantOperatingCityId extractReq
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully extracted PAN data"
          return $ Just (ExtractedPan extractedResp)
      )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to extract PAN: " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
          return Nothing
    (Just _pdfBytes, DVC.DriverLicense) ->
      ( do
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching and extracting DL data"
          let extractReq =
                DigiTypes.DigiLockerExtractDLReq
                  { accessToken = accessToken,
                    uri = uri
                  }
          logInfo $
            "DigiLocker - DriverId: "
              <> driverId.getId
              <> ", StateId: "
              <> stateId
              <> ", Calling fetchAndExtractVerifiedDL with uri: "
              <> uri
              <> ", merchantOpCityId: "
              <> person.merchantOperatingCityId.getId
          extractedResp <- Verification.fetchAndExtractVerifiedDL person.merchantId person.merchantOperatingCityId extractReq
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully extracted DL data"
          return $ Just (ExtractedDL extractedResp)
      )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to extract DL: " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
          return Nothing
    (Just _pdfBytes, _) -> do
      logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Unsupported document type: " <> show docType
      updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just "UNSUPPORTED_TYPE") (Just $ "Document type not supported")
      return Nothing

  -- Step 4: Verify and store data (with error handling)
  -- For Aadhaar: Raw XML + extracted data
  -- For PAN/DL: PDF + extracted data
  case (mbPdfData, mbAadhaarXml, mbExtractedData) of
    -- Aadhaar special case: Raw XML (not PDF) + extracted data
    (Nothing, Just xmlText, Just (ExtractedAadhaar aadhaarResp)) -> do
      ( do
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing Aadhaar card data (XML only, no PDF)"
          -- Convert XML text to ByteString for storage
          let xmlBytes = BSL.fromStrict (TE.encodeUtf8 xmlText)
          verifyAndStoreAadhaar session person xmlBytes aadhaarResp
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "200") (Just "Aadhaar card verified and stored successfully")
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Aadhaar card verification completed"
        )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verification failed for Aadhaar: " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)

    -- PAN and DL: Both PDF and extracted data required
    (Just pdfBytes, Nothing, Just extractedData) -> do
      -- Process based on extracted data type
      ( do
          case extractedData of
            ExtractedPan panResp -> do
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing PAN card data"
              verifyAndStorePAN session person pdfBytes panResp
              updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "200") (Just "PAN card verified and stored successfully")
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", PAN card verification completed"
            ExtractedAadhaar _ -> do
              -- This case shouldn't happen (Aadhaar handled above)
              logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Unexpected: Aadhaar with PDF data"
              throwError $ InternalError "Aadhaar should not have PDF data"
            ExtractedDL dlResp -> do
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing Driver License data"
              verifyAndStoreDL session person pdfBytes dlResp
              updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "200") (Just "Driver License verified and stored successfully")
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Driver License verification completed"
        )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          -- Handle DigiLocker-specific errors during verification
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verification failed for " <> show docType <> ": " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
    _ -> do
      -- PDF/XML or extraction failed, error already logged
      logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Skipping verification for " <> show docType <> " due to fetch/extraction failures"

-- | Verify and store PAN card data
-- Extracts data from DigiLocker, uploads PDF to S3, and calls existing PAN registration logic
verifyAndStorePAN ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString -> -- PDF data
  VerificationTypes.ExtractedDigiLockerPanResp -> -- Extracted PAN response
  Flow ()
verifyAndStorePAN session person pdfBytes extractedPan = do
  let stateId = session.stateId
  -- Step 1: Extract PAN data from DigiLocker response
  panFlow <-
    extractedPan.extractedPan
      & fromMaybeM (InternalError "PAN data not found in DigiLocker response")

  panNumber <-
    panFlow.pan
      & fromMaybeM (InternalError "PAN number not found in DigiLocker XML")

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted PAN data: PAN=" <> panNumber <> ", Name=" <> show panFlow.name

  -- Step 2: Check for duplicate PAN BEFORE uploading to S3 (avoid waste)
  mbExistingPan <- QDPC.findUnInvalidByPanNumber panNumber
  whenJust mbExistingPan $ \existingPan -> do
    when (existingPan.driverId /= person.id) $
      throwError $ DocumentAlreadyLinkedToAnotherDriver "PAN"
    when (existingPan.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "PAN"
    when (existingPan.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "PAN"

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No duplicate PAN found, proceeding with upload"

  -- Step 3: Upload PDF to S3 via validateImage
  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.PanCard,
            rcNumber = Nothing,
            validationStatus = Nothing, -- DigiLocker docs don't need SDK validation
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded PAN PDF to S3, ImageId: " <> imageId.getId

  -- Step 4: Build DriverPanReq and call existing registration logic
  -- Duplicate check already done above, so this should succeed
  let panReq =
        APITypes.DriverPanReq
          { panNumber = panNumber,
            imageId1 = imageId,
            imageId2 = Nothing,
            consent = True,
            consentTimestamp = Nothing, -- Will use current time
            dateOfBirth = panFlow.dob >>= parsePanDob,
            nameOnCard = panFlow.name,
            nameOnGovtDB = panFlow.name,
            docType = Nothing, -- Individual PAN
            transactionId = Nothing, -- No HyperVerge transaction for DigiLocker
            validationStatus = Nothing, -- Not using SDK
            verifiedBy = Just DPC.DIGILOCKER
          }

  -- Call existing PAN registration logic (handles duplicates, validation, storage)
  void $
    DriverOnboardingV2.postDriverRegisterPancardHelper
      (Just person.id, person.merchantId, person.merchantOperatingCityId)
      False -- isDashboard = False
      panReq

  logInfo $ "DigiLocker - Successfully stored PAN card via postDriverRegisterPancardHelper for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

-- | Verify and store Aadhaar card data
-- Extracts data from DigiLocker, uploads raw XML to S3, and calls split Aadhaar registration logic
-- Note: Aadhaar doesn't support getFile API (no PDF), so we store raw XML instead
verifyAndStoreAadhaar ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString -> -- Raw XML bytes from DigiLocker
  VerificationTypes.ExtractedDigiLockerAadhaarResp -> -- Extracted Aadhaar response
  Flow ()
verifyAndStoreAadhaar session person xmlBytes extractedAadhaar = do
  let stateId = session.stateId
  -- Step 1: Extract Aadhaar data from DigiLocker response
  aadhaarFlow <-
    extractedAadhaar.extractedAadhaar
      & fromMaybeM (InternalError "Aadhaar data not found in DigiLocker response")

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted Aadhaar data: Name=" <> show aadhaarFlow.fullName <> ", DOB=" <> show aadhaarFlow.dob

  -- Step 2: Check for existing Aadhaar BEFORE uploading to S3 (avoid waste)
  -- Using the split validation function
  DriverOnboardingV2.validateAadhaarChecks person.id

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No duplicate Aadhaar found, proceeding with upload"

  -- Step 3: Upload raw XML to S3 via validateImage
  -- Convert XML bytes to base64 (validateImage expects base64 text, then stores in S3)
  let xmlBase64 = base64Encode (BSL.toStrict xmlBytes)

  let imageReq =
        Image.ImageValidateRequest
          { image = xmlBase64,
            imageType = DVC.AadhaarCard,
            rcNumber = Nothing,
            validationStatus = Nothing, -- DigiLocker docs don't need SDK validation
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "xml"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded Aadhaar XML to S3, ImageId: " <> imageId.getId

  -- Step 4: Build AadhaarCardReq and call split CRUD function
  -- DigiLocker data is already verified, so status is VALID (AUTO_APPROVED)
  now <- getCurrentTime
  let aadhaarReq =
        APITypes.AadhaarCardReq
          { aadhaarFrontImageId = Just imageId, -- Stores JSON representation
            aadhaarBackImageId = Nothing, -- DigiLocker provides single data source, not front/back images
            maskedAadhaarNumber = aadhaarFlow.idNumber,
            nameOnCard = aadhaarFlow.fullName,
            dateOfBirth = aadhaarFlow.dob,
            address = aadhaarFlow.address,
            consent = True, -- Already granted via DigiLocker
            consentTimestamp = now,
            transactionId = "DIGILOCKER", -- Placeholder - not used in DigiLocker flow
            validationStatus = APITypes.AUTO_APPROVED -- DigiLocker verified
          }

  DriverOnboardingV2.createAadhaarRecord
    person.id
    person.merchantId
    person.merchantOperatingCityId
    aadhaarReq

  logInfo $ "DigiLocker - Successfully stored Aadhaar card for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

-- | Verify and store Driver License data
-- Extracts data from DigiLocker, uploads PDF to S3, and calls existing DL registration logic
verifyAndStoreDL ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString -> -- PDF data
  VerificationTypes.ExtractedDigiLockerDLResp -> -- Extracted DL response
  Flow ()
verifyAndStoreDL session person pdfBytes extractedDL = do
  let stateId = session.stateId
  -- Step 1: Extract DL data from DigiLocker response
  dlFlow <-
    extractedDL.extractedDL
      & fromMaybeM (InternalError "DL data not found in DigiLocker response")

  dlNumber <-
    dlFlow.dlNumber
      & fromMaybeM (InternalError "DL number not found in DigiLocker XML")

  dlExpiry <-
    case dlFlow.expiryDate of
      Just val -> pure val
      Nothing -> throwError $ InternalError "DL expiry not found in DigiLocker XML"

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted DL data: DL=" <> dlNumber <> ", Name=" <> show dlFlow.name <> ", Expiry=" <> show dlExpiry

  -- Step 2: Validate age (18-80 years) if DOB is present
  now <- getCurrentTime
  whenJust dlFlow.dob $ \dobText -> do
    -- Parse DOB text to UTCTime (format: DD-MM-YYYY from DigiLocker)
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobText) of
      Just dobUTC -> do
        let age = diffInYears dobUTC now
        when (age < 18 || age > 80) $
          throwError $ InvalidRequest $ "Driver age must be between 18 and 80 years. Current age: " <> show age
      Nothing -> logWarning $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Could not parse DOB: " <> dobText

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

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No blocking duplicate DL found, proceeding with upload"

  -- Step 4: Upload PDF to S3 via validateImage
  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.DriverLicense,
            rcNumber = Nothing,
            validationStatus = Nothing, -- DigiLocker docs don't need SDK validation
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded DL PDF to S3, ImageId: " <> imageId.getId

  -- Step 5: Get vehicle category from session (driver selected this during /initiate)
  let vehicleCategory = session.vehicleCategory
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Using vehicle category from session: " <> show vehicleCategory

  -- Step 6: Get DocumentVerificationConfig for validation
  -- This config contains the supported COVs for the selected vehicle category
  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory
      person.merchantOperatingCityId
      DVC.DriverLicense
      vehicleCategory
      Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DVC.DriverLicense <> " for category " <> show vehicleCategory))

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Retrieved DocumentVerificationConfig for category: " <> show vehicleCategory

  -- Step 7: Convert extracted data to format expected by onVerifyDLHandler
  -- Convert COVs from [Text] to [Idfy.CovDetail]
  let covDetails = createCovDetails <$> dlFlow.classOfVehicles
  -- DigiLocker returns dates as DD-MM-YYYY; normalize to YYYY-MM-DD
  normalizedExpiryDay <-
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dlExpiry) :: Maybe Day of
      Just day -> pure day
      Nothing -> do
        logError $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Unable to normalize DL expiry; original value: " <> dlExpiry
        throwError $ InternalError "Unable to parse DigiLocker DL expiry date"
  let normalizedExpiryText = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" normalizedExpiryDay
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Normalized DL expiry to: " <> normalizedExpiryText
  let dobText = dlFlow.dob

  -- Parse dateOfIssue from Text to UTCTime (onVerifyDLHandler expects Maybe UTCTime)
  let dateOfIssueUTC =
        dlFlow.dateOfIssue >>= \dateText ->
          parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateText)

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Calling onVerifyDLHandler with COVs: " <> show dlFlow.classOfVehicles <> ", VehicleCategory: " <> show vehicleCategory

  -- Step 8: Call onVerifyDLHandler to validate and store DL
  -- This will validate that DL's COVs match the selected vehicle category
  DLModule.onVerifyDLHandler
    person
    (Just dlNumber)
    (Just normalizedExpiryText)
    covDetails
    dlFlow.name
    dobText
    documentVerificationConfig
    imageId
    Nothing -- imageId2
    dlFlow.name -- nameOnCard
    dateOfIssueUTC -- dateOfIssue parsed to UTCTime
    (Just vehicleCategory) -- vehicleCategory from session (driver's selection)
  logInfo $ "DigiLocker - Successfully stored DL via onVerifyDLHandler for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

-- | Update a single document's status in DocStatusMap
updateDocStatusField ::
  Id DDV.DigilockerVerification ->
  DVC.DocumentType ->
  Text -> -- status text (e.g., "PENDING", "SUCCESS", "FAILED")
  Maybe Text -> -- response code
  Maybe Text -> -- response description
  Flow ()
updateDocStatusField sessionId docType statusText respCode respDesc = do
  -- Fetch current session
  mbSession <- QDV.findById sessionId
  whenJust mbSession $ \session -> do
    -- Parse status text to DocStatusEnum
    docStatusEnum <-
      case DocStatus.textToDocStatusEnum statusText of
        Just s -> pure s
        Nothing -> do
          logError $ "DigiLocker - DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", Failed to parse status: " <> statusText
          throwError $ InternalError $ "Invalid status: " <> statusText

    -- Get current DocStatusMap (already strongly-typed)
    let currentDocStatusMap = session.docStatus

    -- Update the DocStatusMap
    let updatedDocStatusMap = DocStatus.updateDocStatus docType docStatusEnum respCode respDesc currentDocStatusMap

    -- Update in database (DocStatusMap is handled directly by Beam)
    QDV.updateDocStatus updatedDocStatusMap sessionId
    logInfo $ "DigiLocker - DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", Updated docStatus for " <> show docType <> " to " <> statusText

-- NOTE: updateDocStatusOnError function removed
-- Individual document errors are handled within processDocuments with proper DB updates
-- Fork-level catastrophic errors only update session status, not docStatus (DB might be broken)

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

----------- Document Validation Helpers -----------

-- | Parse PAN DOB format (DD-MM-YYYY) to UTCTime
parsePanDob :: Text -> Maybe UTCTime
parsePanDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

-- | Parse Aadhaar DOB format (DDMMYYYY) to UTCTime
parseAadhaarDob :: Text -> Maybe UTCTime
parseAadhaarDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d%m%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

-- | Parse DL DOB format (DD-MM-YYYY) to UTCTime
parseDLDob :: Text -> Maybe UTCTime
parseDLDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

-- | Calculate age difference in years between two UTCTime values
diffInYears :: UTCTime -> UTCTime -> Integer
diffInYears dob now =
  let days = diffDays (utctDay now) (utctDay dob)
   in days `div` 365

-- | Format UTCTime to DigiLocker date string format (DD-MM-YYYY)
formatUTCToDateString :: UTCTime -> Text
formatUTCToDateString utcTime =
  T.pack $ formatTime defaultTimeLocale "%d-%m-%Y" utcTime

-- | Convert list of COV texts to Idfy.CovDetail format
-- DigiLocker provides COVs as [Text], but onVerifyDLHandler expects [Idfy.CovDetail]
createCovDetails :: [Text] -> [Idfy.CovDetail]
createCovDetails covs = map createCovDetail covs
  where
    createCovDetail :: Text -> Idfy.CovDetail
    createCovDetail covText =
      Idfy.CovDetail
        { Idfy.category = Nothing, -- DigiLocker doesn't provide COV category
          Idfy.cov = covText,
          Idfy.issue_date = Nothing -- DigiLocker doesn't provide per-COV issue dates
        }

-- | Convert ByteString to base64-encoded Text
base64Encode :: BS.ByteString -> Text
base64Encode = TE.decodeUtf8 . B64.encode
