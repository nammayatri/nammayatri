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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as VehicleCategory
import Environment
import Kernel.External.Encryption (decrypt, encrypt, unEncrypted)
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
import qualified Storage.Queries.DigilockerVerificationExtra as QDVExtra
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverLicenseExtra as QDLE
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Verification as Verification

type DigiLockerCallbackAPI =
  "verify"
    :> "callback"
    :> "digiLocker"
    :> QueryParam "error" Text
    :> QueryParam "error_description" Text
    :> QueryParam "code" Text
    :> MandatoryQueryParam "state" Text
    :> Get '[JSON] AckResponse

digiLockerCallbackHandler ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  Flow AckResponse
digiLockerCallbackHandler mbError mbErrorDescription mbCode stateParam = do
  logInfo $ "DigiLocker OAuth Callback Received - State: " <> stateParam

  when (T.null stateParam) $ do
    logError "DigiLocker callback - Missing required state parameter. Cannot identify driver."
    throwError DigiLockerEmptyStateParameter

  mbSession <- QDV.findByStateId stateParam
  let driverIdStr = case mbSession of
        Just session -> ", DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId
        Nothing -> ", StateId: " <> stateParam <> " (driver not found)"

  whenJust mbError $ \errorCode -> do
    let errorMsg = fromMaybe "Unknown OAuth error" mbErrorDescription
    logError $ "DigiLocker OAuth Error - Code: " <> errorCode <> ", Description: " <> errorMsg <> driverIdStr

    whenJust mbSession $ \session -> do
      if errorCode == "access_denied"
        then do
          QDV.updateSessionStatus DDV.CONSENT_DENIED (Just errorCode) (Just errorMsg) session.id
          logInfo $ "DigiLocker callback - Updated session status to CONSENT_DENIED for DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId
        else do
          QDV.updateSessionStatus DDV.FAILED (Just errorCode) (Just errorMsg) session.id
          logInfo $ "DigiLocker callback - Updated session status to FAILED for DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", ErrorCode: " <> errorCode

    throwError $ DigiLockerOAuthError errorCode errorMsg

  code <- case mbCode of
    Nothing -> do
      logError $ "DigiLocker callback - Missing authorization code" <> driverIdStr
      whenJust mbSession $ \session -> do
        QDV.updateSessionStatus DDV.FAILED (Just "MISSING_CODE") (Just "DigiLocker callback received without authorization code") session.id
        logInfo $ "DigiLocker callback - Updated session status to FAILED for DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", ErrorCode: MISSING_CODE"
      throwError DigiLockerMissingAuthorizationCode
    Just codeVal ->
      if T.null codeVal
        then do
          logError $ "DigiLocker callback - Authorization code is empty" <> driverIdStr
          whenJust mbSession $ \session -> do
            QDV.updateSessionStatus DDV.FAILED (Just "EMPTY_CODE") (Just "DigiLocker callback received with empty authorization code") session.id
            logInfo $ "DigiLocker callback - Updated session status to FAILED for DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", ErrorCode: EMPTY_CODE"
          throwError DigiLockerEmptyAuthorizationCode
        else return codeVal

  session <- QDV.findByStateId stateParam >>= fromMaybeM DigiLockerInvalidStateParameter
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker callback - Found session for DriverId: " <> driverId.getId <> ", StateId: " <> stateId

  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  transporterConfig <-
    CQTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $ do
    logError $ "DigiLocker callback - DigiLocker not enabled for merchantOpCityId: " <> person.merchantOperatingCityId.getId
    throwError DigiLockerNotEnabled

  digiLockerConfig <- SDDigilocker.getDigiLockerConfig person.merchantOperatingCityId
  logInfo $ "DigiLocker callback - Loaded DigiLocker config for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", merchantOpCityId: " <> person.merchantOperatingCityId.getId <> ", baseUrl: " <> show digiLockerConfig.url

  let tokenReq =
        InterfaceTypes.TokenizationReq
          { code = Just code,
            codeVerifier = Just session.codeVerifier,
            expiry = Nothing
          }

  encryptedSecret <- encrypt digiLockerConfig.clientSecret
  let tokenizeConfig =
        TokenizeTypes.DigilockerTokenizeConfig
          { url = digiLockerConfig.url,
            clientId = digiLockerConfig.clientId,
            clientSecret = encryptedSecret,
            redirectUri = digiLockerConfig.redirectUri
          }

  logInfo $ "DigiLocker callback - Calling Tokenize.tokenize for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", codeVerifier present: " <> show (isJust tokenReq.codeVerifier) <> ", state: present"
  tokenResp <-
    Tokenize.tokenize (InterfaceTypes.DigilockerTokenizationServiceConfig tokenizeConfig) tokenReq
      `catchAny` \err -> do
        logError $ "DigiLocker callback - Token API failed for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ". Error: " <> show err
        QDV.updateSessionStatus DDV.FAILED (Just "TOKEN_API_FAILED") (Just $ T.pack $ show err) session.id
        throwError DigiLockerTokenExchangeFailed

  logInfo $ "DigiLocker callback - Token API success for DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ". Access token obtained, scope: " <> show tokenResp.scope

  let accessTokenPlain = tokenResp.token
  let scope = tokenResp.scope
  let expiresAt = tokenResp.expiresAt

  logInfo $ "DigiLocker callback - Before encryption, accessToken (plain, first 20 chars): " <> T.take 20 accessTokenPlain <> "..."
  accessTokenEncrypted <- encrypt accessTokenPlain
  logInfo $ "DigiLocker callback - After encryption, accessToken encrypted (first 20 chars): " <> T.take 20 (unEncrypted accessTokenEncrypted) <> "..."
  QDVExtra.updateAccessToken (Just accessTokenEncrypted) expiresAt (Just code) scope stateParam

  requiredDocs <- getRequiredDocuments person.merchantOperatingCityId session.vehicleCategory
  alreadyVerifiedDocs <- getAlreadyVerifiedDocuments driverId
  let unavailableDocs = filter (`notElem` alreadyVerifiedDocs) (map (.documentType) requiredDocs)

  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Required docs: " <> show (map (.documentType) requiredDocs)
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Already verified: " <> show alreadyVerifiedDocs
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Unavailable docs: " <> show unavailableDocs

  let scopeText = fromMaybe "" scope
  let parsedDocs = parseDigiLockerScope scopeText

  let (consentGranted, consentDenied) = checkDocumentConsent unavailableDocs parsedDocs

  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Consent granted for: " <> show consentGranted
  logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Consent denied for: " <> show consentDenied

  if not (null consentDenied)
    then do
      QDV.updateSessionStatus DDV.CONSENT_DENIED Nothing (Just $ "Missing consent for: " <> T.intercalate ", " (map show consentDenied)) session.id
      logWarning $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Missing consent for required documents: " <> show consentDenied
      logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Scope stored for debugging. Parse scope to see granted vs denied docs."
      return Ack
    else do
      let initialDocStatus = initializeDocStatus unavailableDocs parsedDocs
      QDV.updateDocStatus initialDocStatus session.id
      logInfo $ "DigiLocker callback - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Initialized docStatus for " <> show (length unavailableDocs) <> " documents"

      QDV.updateSessionStatus DDV.SUCCESS Nothing Nothing session.id

      accessTokenPlainDecrypted <- decrypt accessTokenEncrypted
      fork "digilocker-verify-documents" $ do
        processDocuments session person digiLockerConfig accessTokenPlainDecrypted parsedDocs unavailableDocs
          `catchAny` \err -> do
            logError $ "DigiLocker - Catastrophic failure in background job - Error: " <> show err <> ", DriverId: " <> driverId.getId <> ", StateId: " <> stateId
            QDV.updateSessionStatus DDV.FAILED (Just "FORK_FAILED") (Just $ T.pack $ show err) session.id

      logInfo $ "DigiLocker callback - Successfully completed callback processing for DriverId: " <> driverId.getId <> ", StateId: " <> stateId
      return Ack

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

data DocumentAvailability
  = Issued Text
  | PullRequired
  | ConsentDenied
  deriving (Show, Eq)

data ExtractedDocumentData
  = ExtractedPan VerificationTypes.ExtractedDigiLockerPanResp
  | ExtractedAadhaar VerificationTypes.ExtractedDigiLockerAadhaarResp
  | ExtractedDL VerificationTypes.ExtractedDigiLockerDLResp
  deriving (Show)

parseDigiLockerScope :: Text -> [(DVC.DocumentType, DocumentAvailability)]
parseDigiLockerScope scopeText =
  let tokens = T.words scopeText
      parsedDocs = mapMaybe parseToken tokens
   in parsedDocs
  where
    parseToken :: Text -> Maybe (DVC.DocumentType, DocumentAvailability)
    parseToken token
      | "issued/" `T.isPrefixOf` token = do
        docType <- extractDocType token
        let uri = fromMaybe token (T.stripPrefix "issued/" token)
        return (docType, Issued uri)
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

checkDocumentConsent ::
  [DVC.DocumentType] ->
  [(DVC.DocumentType, DocumentAvailability)] ->
  ([DVC.DocumentType], [DVC.DocumentType])
checkDocumentConsent requiredDocs parsedDocs =
  let availableDocs = map fst parsedDocs
      granted = filter (`elem` availableDocs) requiredDocs
      denied = filter (`notElem` availableDocs) requiredDocs
   in (granted, denied)

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

processDocuments ::
  DDV.DigilockerVerification ->
  DP.Person ->
  DigilockerTypes.DigiLockerCfg ->
  Text ->
  [(DVC.DocumentType, DocumentAvailability)] ->
  [DVC.DocumentType] ->
  Flow ()
processDocuments session person _digiLockerConfig accessToken parsedDocs unavailableDocs = do
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Starting document processing for " <> show (length unavailableDocs) <> " documents"

  let sessionMerchantId = session.merchantId
  let sessionMerchantOpCityId = session.merchantOperatingCityId

  when (sessionMerchantId /= person.merchantId || sessionMerchantOpCityId /= person.merchantOperatingCityId) $ do
    logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Mismatched merchant/city context. Session: merchantId=" <> sessionMerchantId.getId <> ", merchantOpCityId=" <> sessionMerchantOpCityId.getId <> ", Person: merchantId=" <> person.merchantId.getId <> ", merchantOpCityId=" <> person.merchantOperatingCityId.getId
    throwError DigiLockerMismatchedMerchantContext

  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Validated session context: merchantId=" <> sessionMerchantId.getId <> ", merchantOpCityId=" <> sessionMerchantOpCityId.getId

  forM_ unavailableDocs $ \docType -> do
    ( do
        case find (\(dt, _) -> dt == docType) parsedDocs of
          Nothing -> do
            logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", No consent found for docType: " <> show docType
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_CONSENT_DENIED) (Just "NO_CONSENT") (Just "User did not grant consent for this document")
          Just (_, availability) -> do
            case availability of
              PullRequired -> do
                updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_PULL_REQUIRED) Nothing Nothing
                logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Marked " <> show docType <> " as PULL_REQUIRED (user needs to enter details)"
              ConsentDenied -> do
                updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_CONSENT_DENIED) (Just "NO_CONSENT") (Just "User denied consent")
                logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Marked " <> show docType <> " as CONSENT_DENIED"
              Issued uri -> do
                processIssuedDocument session person sessionMerchantId sessionMerchantOpCityId accessToken docType uri
      )
      `catchAny` \err -> do
        logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to process " <> show docType <> ": " <> show err
        updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just "PROCESSING_ERROR") (Just $ T.pack $ show err)

  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Completed processing all documents"

processIssuedDocument ::
  DDV.DigilockerVerification ->
  DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  DVC.DocumentType ->
  Text ->
  Flow ()
processIssuedDocument session person sessionMerchantId sessionMerchantOpCityId accessToken docType uri = do
  let driverId = session.driverId
  let stateId = session.stateId
  logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Processing issued document: " <> show docType <> ", URI: " <> uri

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
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Calling getDigiLockerFile with uri: " <> uri <> ", merchantOpCityId: " <> sessionMerchantOpCityId.getId
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", getDigiLockerFile input: merchantId=" <> sessionMerchantId.getId <> ", merchantOpCityId=" <> sessionMerchantOpCityId.getId <> ", uri=" <> uri
            pdfBytes <- Verification.getDigiLockerFile sessionMerchantId sessionMerchantOpCityId fileReq
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", getDigiLockerFile output bytes: " <> show (BSL.length pdfBytes)
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully fetched PDF for " <> show docType
            return $ Just pdfBytes
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to fetch PDF for " <> show docType <> ": " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing

  mbAadhaarXml <-
    if docType == DVC.AadhaarCard
      then
        ( do
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching raw Aadhaar XML for S3 storage"
            let extractReq =
                  DigiTypes.DigiLockerExtractAadhaarReq
                    { accessToken = accessToken
                    }
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Calling getVerifiedAadhaarXML for merchantOpCityId: " <> sessionMerchantOpCityId.getId
            xmlText <- Verification.getVerifiedAadhaarXML sessionMerchantId sessionMerchantOpCityId extractReq
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully fetched Aadhaar XML"
            return $ Just xmlText
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to fetch Aadhaar XML: " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing
      else return Nothing

  mbExtractedData <- case (mbPdfData, docType) of
    (Nothing, DVC.AadhaarCard)
      | isJust mbAadhaarXml ->
        ( do
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Extracting Aadhaar data from XML"
            let extractReq =
                  DigiTypes.DigiLockerExtractAadhaarReq
                    { accessToken = accessToken
                    }
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Calling fetchAndExtractVerifiedAadhaar for merchantOpCityId: " <> sessionMerchantOpCityId.getId
            extractedResp <- Verification.fetchAndExtractVerifiedAadhaar sessionMerchantId sessionMerchantOpCityId extractReq
            logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Successfully extracted Aadhaar data"
            return $ Just (ExtractedAadhaar extractedResp)
        )
          `catch` \(err :: DigiLockerError.DigiLockerError) -> do
            let (errorCode, errorDesc) = extractDigiLockerError err
            logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Failed to extract Aadhaar: " <> errorCode <> " - " <> errorDesc
            updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
            return Nothing
    (Nothing, _) -> return Nothing
    (Just _pdfBytes, DVC.PanCard) ->
      ( do
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Fetching and extracting PAN data"
          let extractReq =
                DigiTypes.DigiLockerExtractPanReq
                  { accessToken = accessToken,
                    uri = uri
                  }
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Calling fetchAndExtractVerifiedPan with uri: " <> uri <> ", merchantOpCityId: " <> sessionMerchantOpCityId.getId
          extractedResp <- Verification.fetchAndExtractVerifiedPan sessionMerchantId sessionMerchantOpCityId extractReq
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
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Calling fetchAndExtractVerifiedDL with uri: " <> uri <> ", merchantOpCityId: " <> sessionMerchantOpCityId.getId
          extractedResp <- Verification.fetchAndExtractVerifiedDL sessionMerchantId sessionMerchantOpCityId extractReq
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

  case (mbPdfData, mbAadhaarXml, mbExtractedData) of
    (Nothing, Just xmlText, Just (ExtractedAadhaar aadhaarResp)) -> do
      ( do
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing Aadhaar card data (XML only, no PDF)"
          let xmlBytes = BSL.fromStrict (TE.encodeUtf8 xmlText)
          verifyAndStoreAadhaar session person xmlBytes aadhaarResp
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "Document Verification Successfull") (Just "Aadhaar card verified and stored successfully")
          logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Aadhaar card verification completed"
        )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verification failed for Aadhaar: " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
    (Just pdfBytes, Nothing, Just extractedData) -> do
      ( do
          case extractedData of
            ExtractedPan panResp -> do
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing PAN card data"
              verifyAndStorePAN session person pdfBytes panResp
              updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "Document Verification Successfull") (Just "PAN card verified and stored successfully")
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", PAN card verification completed"
            ExtractedAadhaar _ -> do
              logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Unexpected: Aadhaar with PDF data"
              throwError $ InternalError "Aadhaar should not have PDF data"
            ExtractedDL dlResp -> do
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verifying and storing Driver License data"
              verifyAndStoreDL session person pdfBytes dlResp
              updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_SUCCESS) (Just "Document Verification Successfull") (Just "Driver License verified and stored successfully")
              logInfo $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Driver License verification completed"
        )
        `catch` \(err :: DigiLockerError.DigiLockerError) -> do
          let (errorCode, errorDesc) = extractDigiLockerError err
          logError $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Verification failed for " <> show docType <> ": " <> errorCode <> " - " <> errorDesc
          updateDocStatusField session.id docType (DocStatus.docStatusToText DocStatus.DOC_FAILED) (Just errorCode) (Just errorDesc)
    _ -> do
      logWarning $ "DigiLocker - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", Skipping verification for " <> show docType <> " due to fetch/extraction failures"

verifyAndStorePAN ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString ->
  VerificationTypes.ExtractedDigiLockerPanResp ->
  Flow ()
verifyAndStorePAN session person pdfBytes extractedPan = do
  let stateId = session.stateId
  panFlow <-
    extractedPan.extractedPan
      & fromMaybeM (InternalError "PAN data not found in DigiLocker response")

  panNumber <-
    panFlow.pan
      & fromMaybeM (InternalError "PAN number not found in DigiLocker XML")
  let maskedPan = maskText panNumber

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted PAN data: PAN=" <> maskedPan <> ", Name=" <> show panFlow.name

  mbExistingPan <- QDPC.findUnInvalidByPanNumber panNumber
  whenJust mbExistingPan $ \existingPan -> do
    when (existingPan.driverId /= person.id) $
      throwError $ DocumentAlreadyLinkedToAnotherDriver "PAN"
    when (existingPan.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "PAN"
    when (existingPan.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "PAN"

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No duplicate PAN found, proceeding with upload"

  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.PanCard,
            rcNumber = Nothing,
            validationStatus = Nothing,
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False Nothing Nothing (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded PAN PDF to S3, ImageId: " <> imageId.getId

  let panReq =
        APITypes.DriverPanReq
          { panNumber = panNumber,
            imageId1 = imageId,
            imageId2 = Nothing,
            consent = True,
            consentTimestamp = Nothing,
            dateOfBirth = panFlow.dob >>= parsePanDob,
            nameOnCard = panFlow.name,
            nameOnGovtDB = panFlow.name,
            docType = Nothing,
            transactionId = Nothing,
            validationStatus = Nothing,
            verifiedBy = Just DPC.DIGILOCKER
          }

  void $
    DriverOnboardingV2.postDriverRegisterPancardHelper
      (Just person.id, person.merchantId, person.merchantOperatingCityId)
      False
      True
      panReq

  logInfo $ "DigiLocker - Successfully stored PAN card via postDriverRegisterPancardHelper for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

verifyAndStoreAadhaar ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString ->
  VerificationTypes.ExtractedDigiLockerAadhaarResp ->
  Flow ()
verifyAndStoreAadhaar session person xmlBytes extractedAadhaar = do
  let stateId = session.stateId
  aadhaarFlow <-
    extractedAadhaar.extractedAadhaar
      & fromMaybeM (InternalError "Aadhaar data not found in DigiLocker response")

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted Aadhaar data: Name=" <> show aadhaarFlow.fullName <> ", DOB=" <> show aadhaarFlow.dob

  DriverOnboardingV2.validateAadhaarChecks person.id

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No duplicate Aadhaar found, proceeding with upload"

  let xmlBase64 = base64Encode (BSL.toStrict xmlBytes)

  let imageReq =
        Image.ImageValidateRequest
          { image = xmlBase64,
            imageType = DVC.AadhaarCard,
            rcNumber = Nothing,
            validationStatus = Nothing,
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "xml"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False Nothing Nothing (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded Aadhaar XML to S3, ImageId: " <> imageId.getId

  now <- getCurrentTime
  let aadhaarReq =
        APITypes.AadhaarCardReq
          { aadhaarFrontImageId = Just imageId,
            aadhaarBackImageId = Nothing,
            maskedAadhaarNumber = aadhaarFlow.idNumber,
            nameOnCard = aadhaarFlow.fullName,
            dateOfBirth = aadhaarFlow.dob,
            address = aadhaarFlow.address,
            consent = True,
            consentTimestamp = now,
            transactionId = "DIGILOCKER",
            validationStatus = APITypes.AUTO_APPROVED
          }

  DriverOnboardingV2.createAadhaarRecord
    person.id
    person.merchantId
    person.merchantOperatingCityId
    aadhaarReq

  logInfo $ "DigiLocker - Successfully stored Aadhaar card for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

verifyAndStoreDL ::
  DDV.DigilockerVerification ->
  DP.Person ->
  BSL.ByteString ->
  VerificationTypes.ExtractedDigiLockerDLResp ->
  Flow ()
verifyAndStoreDL session person pdfBytes extractedDL = do
  let stateId = session.stateId
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

  let maskedDl = maskText dlNumber

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Extracted DL data: DL=" <> maskedDl <> ", Name=" <> show dlFlow.name <> ", Expiry=" <> show dlExpiry

  now <- getCurrentTime
  whenJust dlFlow.dob $ \dobText -> do
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobText) of
      Just dobUTC -> do
        let age = diffInYears dobUTC now
        when (age < 18 || age > 80) $
          throwError DigiLockerInvalidDriverAge
      Nothing -> logWarning $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Could not parse DOB: " <> dobText

  mbExistingDL <- QDLE.findByDLNumber dlNumber
  whenJust mbExistingDL $ \existingDL -> do
    when (existingDL.driverId /= person.id) $
      throwError $ DocumentAlreadyLinkedToAnotherDriver "DL"
    when (existingDL.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "DL"
    when (existingDL.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "DL"

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", No blocking duplicate DL found, proceeding with upload"

  let pdfBase64 = base64Encode (BSL.toStrict pdfBytes)
  let imageReq =
        Image.ImageValidateRequest
          { image = pdfBase64,
            imageType = DVC.DriverLicense,
            rcNumber = Nothing,
            validationStatus = Nothing,
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing,
            fileExtension = Just "pdf"
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False Nothing Nothing (person.id, person.merchantId, person.merchantOperatingCityId) imageReq
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Uploaded DL PDF to S3, ImageId: " <> imageId.getId

  let vehicleCategory = session.vehicleCategory
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Using vehicle category from session: " <> show vehicleCategory

  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory
      person.merchantOperatingCityId
      DVC.DriverLicense
      vehicleCategory
      Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DVC.DriverLicense <> " for category " <> show vehicleCategory))

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Retrieved DocumentVerificationConfig for category: " <> show vehicleCategory

  let covDetails = createCovDetails <$> dlFlow.classOfVehicles
  normalizedExpiryDay <-
    case parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dlExpiry) :: Maybe Day of
      Just day -> pure day
      Nothing -> do
        logError $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Unable to normalize DL expiry; original value: " <> dlExpiry
        throwError $ InternalError "Unable to parse DigiLocker DL expiry date"
  let normalizedExpiryText = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" normalizedExpiryDay
  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Normalized DL expiry to: " <> normalizedExpiryText
  let dobText = dlFlow.dob

  let dateOfIssueUTC =
        dlFlow.dateOfIssue >>= \dateText ->
          parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateText)

  logInfo $ "DigiLocker - DriverId: " <> person.id.getId <> ", StateId: " <> stateId <> ", Calling onVerifyDLHandler with COVs: " <> show dlFlow.classOfVehicles <> ", VehicleCategory: " <> show vehicleCategory

  DLModule.onVerifyDLHandler
    person
    (Just dlNumber)
    (Just normalizedExpiryText)
    covDetails
    dlFlow.name
    dobText
    documentVerificationConfig
    imageId
    Nothing
    dlFlow.name
    dateOfIssueUTC
    (Just vehicleCategory)
  logInfo $ "DigiLocker - Successfully stored DL via onVerifyDLHandler for DriverId: " <> person.id.getId <> ", StateId: " <> stateId

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
          logError $ "DigiLocker - DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", Failed to parse status: " <> statusText
          throwError $ InternalError $ "Invalid status: " <> statusText

    let currentDocStatusMap = session.docStatus
    let updatedDocStatusMap = DocStatus.updateDocStatus docType docStatusEnum respCode respDesc currentDocStatusMap
    QDV.updateDocStatus updatedDocStatusMap sessionId
    logInfo $ "DigiLocker - DriverId: " <> session.driverId.getId <> ", StateId: " <> session.stateId <> ", Updated docStatus for " <> show docType <> " to " <> statusText

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

parsePanDob :: Text -> Maybe UTCTime
parsePanDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

parseAadhaarDob :: Text -> Maybe UTCTime
parseAadhaarDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d%m%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

parseDLDob :: Text -> Maybe UTCTime
parseDLDob dobStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dobStr)
  return $ UTCTime parsed 0

diffInYears :: UTCTime -> UTCTime -> Integer
diffInYears dob now =
  let days = diffDays (utctDay now) (utctDay dob)
   in days `div` 365

formatUTCToDateString :: UTCTime -> Text
formatUTCToDateString utcTime =
  T.pack $ formatTime defaultTimeLocale "%d-%m-%Y" utcTime

createCovDetails :: [Text] -> [Idfy.CovDetail]
createCovDetails covs = map createCovDetail covs
  where
    createCovDetail :: Text -> Idfy.CovDetail
    createCovDetail covText =
      Idfy.CovDetail
        { Idfy.category = Nothing,
          Idfy.cov = covText,
          Idfy.issue_date = Nothing
        }

base64Encode :: BS.ByteString -> Text
base64Encode = TE.decodeUtf8 . B64.encode
