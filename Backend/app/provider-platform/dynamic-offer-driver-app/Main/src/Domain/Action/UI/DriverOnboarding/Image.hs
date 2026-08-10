{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.Image
  ( ImageValidateRequest (..),
    ImageValidateResponse (..),
    ImageValidateFileRequest (..),
    validateImage,
    validateImageFile,
    getImage,
    getImageWithAccessCheck,
    imageS3Lock,
    throwValidationError,
    convertHVStatusToValidationStatus,
    convertValidationStatusToVerificationStatus,
    -- Exported for unit tests: pure classification helpers with no Flow dependency.
    UploadedFileType (..),
    detectUploadedFileType,
    canonicalExtension,
    allowedExtensionsFor,
    normalizeExtension,
    resolveStoredExtension,
    isImageOnlyDocument,
    pdfActiveContentFinding,
    containsPdfName,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as Domain
-- 'PDF' is hidden because AWS.S3's FileType has a constructor of the same name that would clash
-- with the local 'UploadedFileType' constructor below; the S3 one is not used in this module.
import AWS.S3 as S3 hiding (PDF)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as Domain hiding (SelfieFetchStatus (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Decode, base64Encode)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Verification.Interface as VI
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified SharedLogic.DriverFleetOperatorAssociation as DFOA
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverRCAssociation as QDRCA
import qualified Storage.Queries.FleetRCAssociationExtra as FRCA
import qualified Storage.Queries.Image as Query
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Error
import qualified Tools.Verification as Verification

data ImageValidateRequest = ImageValidateRequest
  { image :: Text,
    imageType :: DVC.DocumentType,
    rcNumber :: Maybe Text, -- for PUC, Permit, Insurance and Fitness
    validationStatus :: Maybe Domain.ValidationStatus,
    workflowTransactionId :: Maybe Text,
    vehicleCategory :: Maybe VehicleCategory,
    sdkFailureReason :: Maybe Text, -- used when frontend sdk is used for extraction.
    fileExtension :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ImageValidateFileRequest = ImageValidateFileRequest
  { image :: FilePath,
    imageType :: DVC.DocumentType,
    rcNumber :: Maybe Text, -- for PUC, Permit, Insurance and Fitness
    validationStatus :: Maybe Domain.ValidationStatus,
    workflowTransactionId :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance FromMultipart Tmp ImageValidateFileRequest where
  fromMultipart form = do
    ImageValidateFileRequest
      <$> fmap fdPayload (lookupFile "image" form)
      <*> fmap (read . T.unpack) (lookupInput "imageType" form)
      <*> parseMaybeInput "rcNumber" form
      <*> parseMaybeInput "validationStatus" form
      <*> parseMaybeInput "workflowTransactionId" form

parseMaybeInput :: Read b => Text -> MultipartData tag -> Either String (Maybe b)
parseMaybeInput fieldName form = case lookupInput fieldName form of
  Right val -> Right $ readMaybe (T.unpack val)
  Left _ -> Right Nothing

newtype ImageValidateResponse = ImageValidateResponse
  {imageId :: Id Domain.Image}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data GetDocsResponse = GetDocsResponse
  { dlImage :: Maybe Text,
    rcImage :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

-- | Formats the onboarding document pipeline is permitted to store.
--
-- Scope note: matching magic bytes is an integrity check on the declared type, not by itself a
-- defence against active content — a polyglot can be a valid JPEG and valid HTML simultaneously.
-- PDF is on this list because GST certificates need it, and PDF supports embedded JavaScript, so
-- PDFs additionally go through 'pdfActiveContentFinding' below. These documents are served back
-- to the dashboard as base64 in the response body rather than as a URL, so there is no S3
-- Content-Disposition or CDN header in the path to fall back on: what is stored is what the
-- viewer renders. Rejecting at upload is therefore the only server-side control available.
data UploadedFileType
  = JPEG
  | PNG
  | WEBP
  | HEIC
  | AVIF
  | PDF
  deriving (Show, Eq)

-- | Identify the payload from its magic bytes. The declared extension is not trusted as an
-- assertion about the content; it is consulted only to disambiguate ISO-BMFF generic brands,
-- which genuinely carry no format information.
detectUploadedFileType :: Maybe Text -> BS.ByteString -> Maybe UploadedFileType
detectUploadedFileType mbDeclaredExtension bs
  | "\xFF\xD8\xFF" `BS.isPrefixOf` bs = Just JPEG
  | "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" `BS.isPrefixOf` bs = Just PNG
  | "%PDF-" `BS.isPrefixOf` bs = Just PDF
  | "RIFF" `BS.isPrefixOf` bs && "WEBP" `BS.isPrefixOf` BS.drop 8 bs = Just WEBP
  -- ISO-BMFF branded container: bytes 4..7 are "ftyp", the brand follows at 8..11. AVIF and HEIC
  -- share this container, so the brand decides which.
  | "ftyp" `BS.isPrefixOf` BS.drop 4 bs =
    let brand = BS.take 4 (BS.drop 8 bs)
     in if
            | brand `elem` ["avif", "avis"] -> Just AVIF
            | brand `elem` ["heic", "heix", "hevc", "hevx", "heim", "heis"] -> Just HEIC
            -- mif1/msf1 are the generic ISO brands, emitted by both AVIF and HEIC encoders. The
            -- bytes cannot distinguish them, so fall back to what the caller declared rather
            -- than picking one and producing a spurious extension mismatch.
            | brand `elem` ["mif1", "msf1"] ->
              Just $ if mbDeclaredExtension == Just "avif" then AVIF else HEIC
            | otherwise -> Nothing
  | otherwise = Nothing

canonicalExtension :: UploadedFileType -> Text
canonicalExtension = \case
  JPEG -> "jpg"
  PNG -> "png"
  WEBP -> "webp"
  HEIC -> "heic"
  AVIF -> "avif"
  PDF -> "pdf"

-- | Extensions a caller may legitimately declare for a given detected format. A declared
-- extension that disagrees with the content is treated as an attempt to smuggle a payload past
-- extension-based checks further down the chain.
allowedExtensionsFor :: UploadedFileType -> [Text]
allowedExtensionsFor = \case
  JPEG -> ["jpg", "jpeg"]
  PNG -> ["png"]
  WEBP -> ["webp"]
  HEIC -> ["heic", "heif"]
  AVIF -> ["avif"]
  PDF -> ["pdf"]

isImageFileType :: UploadedFileType -> Bool
isImageFileType = \case
  PDF -> False
  _ -> True

-- | Document types that are captured as photographs and rendered directly in an <img>. A PDF
-- stored under one of these would be stored as .pdf and break every consumer rendering it, so
-- these accept image formats only. Everything else (certificates, licences, forms) may be either.
isImageOnlyDocument :: DVC.DocumentType -> Bool
isImageOnlyDocument docType =
  docType
    `elem` [ DVC.ProfilePhoto,
             DVC.UploadProfile,
             DVC.VehicleFront,
             DVC.VehicleBack,
             DVC.VehicleRight,
             DVC.VehicleLeft,
             DVC.VehicleFrontInterior,
             DVC.VehicleBackInterior,
             DVC.Odometer,
             DVC.InspectionHub
           ]

-- | PDF names that make a viewer do something other than display a page. A scanned certificate
-- has no reason to carry any of these, and each is a documented execution or exfiltration vector.
--
-- Deliberately excludes @/OpenAction@ and @/AA@: both are routinely present in benign documents
-- to set an initial zoom or page, and are only dangerous when they reference a @/JavaScript@
-- action, which is itself on this list. Also excludes @/URI@, which is an ordinary hyperlink.
pdfActiveContentNames :: [BS.ByteString]
pdfActiveContentNames =
  [ "/JavaScript",
    "/JS",
    "/Launch",
    "/EmbeddedFile",
    "/EmbeddedFiles",
    "/XFA",
    "/RichMedia",
    "/SubmitForm",
    "/ImportData"
  ]

-- | The first active-content name present in a PDF, if any.
--
-- Best effort by construction, and worth being explicit about the limits. PDF allows these names
-- to live inside a compressed object stream (@/ObjStm@ with a @/FlateDecode@ filter), where a raw
-- byte scan cannot see them, and allows them to be written with hex escapes such as
-- @/J#61vaScript@. This catches the direct encodings, which is what commodity PDF-payload
-- generators emit. It raises the cost of the attack; it does not make a stored PDF trustworthy,
-- so the viewer rendering these must still treat them as untrusted.
pdfActiveContentFinding :: BS.ByteString -> Maybe BS.ByteString
pdfActiveContentFinding bs = find (`containsPdfName` bs) pdfActiveContentNames

-- | Whether a PDF name token occurs, respecting token boundaries. The boundary check is what
-- keeps @/JS@ from also matching the @/JSName@ of some unrelated dictionary key.
containsPdfName :: BS.ByteString -> BS.ByteString -> Bool
containsPdfName name = go
  where
    go haystack =
      let (_, rest) = BS.breakSubstring name haystack
       in not (BS.null rest)
            && case BS.uncons (BS.drop (BS.length name) rest) of
              -- Name runs to end of file: nothing can follow, so the token is complete.
              Nothing -> True
              Just (c, _) -> isPdfDelimiter c || go (BS.drop 1 rest)
    -- PDF whitespace and delimiter characters (ISO 32000-1 tables 1 and 2).
    isPdfDelimiter c = c `BS.elem` "\0\t\n\f\r ()<>[]{}/%"

-- | Reject anything whose bytes are not a permitted document format, whose declared extension
-- contradicts those bytes, which is a PDF under a photo-only document type, or which is a PDF
-- carrying active content.
--
-- When @enforce@ is False this runs in shadow mode: every rejection is logged and allowed
-- through, so the rejection rate can be measured against real onboarding traffic before the
-- check starts failing requests. Returns Nothing when the content could not be identified,
-- in which case the caller falls back to the legacy declared-extension behaviour.
validateUploadedFileType ::
  MonadFlow m =>
  Bool ->
  DVC.DocumentType ->
  BS.ByteString ->
  Maybe Text ->
  m (Maybe UploadedFileType)
validateUploadedFileType enforce docType content mbDeclaredExtension = do
  let mbDeclared = normalizeExtension =<< mbDeclaredExtension
  case detectUploadedFileType mbDeclared content of
    Nothing ->
      reject ("Unrecognized file content, declared extension: " <> show mbDeclared) Nothing $
        "Unsupported file type. Only JPEG, PNG, WEBP, HEIC, AVIF and PDF documents are accepted."
    Just fileType -> do
      let extensionMismatch = case mbDeclared of
            Just declared -> declared `notElem` allowedExtensionsFor fileType
            Nothing -> False
      if extensionMismatch
        then
          reject ("Declared extension " <> show mbDeclared <> " contradicts detected " <> show fileType) (Just fileType) $
            "File extension does not match the uploaded file content."
        else
          if isImageOnlyDocument docType && not (isImageFileType fileType)
            then
              reject (show fileType <> " uploaded for image-only document " <> show docType) (Just fileType) $
                show docType <> " must be an image, not a " <> T.toLower (show fileType) <> "."
            else case (fileType, pdfActiveContentFinding content) of
              -- Matched left-to-right, so the scan is only forced for a PDF.
              (PDF, Just marker) ->
                reject ("PDF carries active content name " <> show marker) (Just fileType) $
                  "This PDF contains scripts, embedded files or interactive forms and cannot be accepted. Please upload a plain scanned document."
              _ -> pure (Just fileType)
  where
    reject logDetail mbFileType userMessage
      | enforce = throwError $ InvalidRequest userMessage
      | otherwise = do
        logWarning $ "UploadFileTypeCheck:shadow: would have rejected upload. " <> logDetail
        pure mbFileType

normalizeExtension :: Text -> Maybe Text
normalizeExtension ext = do
  let normalized = ext & T.strip & T.dropWhile (== '.') & T.toLower
  if T.null normalized then Nothing else Just normalized

-- | Extension the object is stored under.
--
-- Shadow mode must be observably inert: with @enforce@ False the object keeps the extension the
-- caller declared, byte-for-byte the behaviour that predates this check, and the only effect of
-- the check is a log line. Letting detected content win while the flag is off would silently
-- change stored S3 paths (a JPEG declared .png would land as .jpg) on every merchant at deploy
-- time, before anyone opted in.
--
-- Only when enforcing does detected content decide the extension.
resolveStoredExtension :: Bool -> Maybe UploadedFileType -> Maybe Text -> Text
resolveStoredExtension enforce mbFileType mbDeclaredExtension
  | enforce, Just fileType <- mbFileType = canonicalExtension fileType
  | otherwise = fromMaybe "png" (normalizeExtension =<< mbDeclaredExtension)

createPath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3Env m)) =>
  Text ->
  Text ->
  DVC.DocumentType ->
  Text ->
  m Text
createPath driverId merchantId documentType sanitizedExt = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/driver-onboarding/" <> "org-" <> merchantId <> "/"
        <> driverId
        <> "/"
        <> show documentType
        <> "/"
        <> fileName
        <> "."
        <> sanitizedExt
    )

validateImageHandler ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImageHandler isDashboard mbUploaderRole mbDocConfigs (personId, _, merchantOpCityId) req@ImageValidateRequest {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantId = person.merchantId
  docConfigs <- maybe (getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just imageType, vehicleCategory = Nothing}) Nothing) pure mbDocConfigs
  -- Only restrict when rolesAllowedToUploadDocument is non-empty; Nothing or [] means all roles allowed
  -- When mbUploaderRole is Nothing (e.g. admin not at BPP), allow; only check when uploader role is known
  whenJust (listToMaybe docConfigs >>= (.rolesAllowedToUploadDocument)) $ \allowedRoles ->
    unless (null allowedRoles) $
      whenJust mbUploaderRole $ \uploaderRole ->
        unless (uploaderRole `elem` allowedRoles) $
          throwError (InvalidRequesterRole $ show uploaderRole)
  when (isJust validationStatus && imageType == DVC.ProfilePhoto) $ checkIfGenuineReq merchantId req
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  imageSizeInBytes <- fromMaybeM (InvalidRequest "Failed to decode base64 image") $ base64Decode image
  let maxSizeInBytes = fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB * 1024 * 1024 -- Should be set for all merchants, taking 100 if not set
  when (BS.length imageSizeInBytes > maxSizeInBytes) $
    throwError $ InvalidRequest $ "Image size " <> show (BS.length imageSizeInBytes) <> " bytes exceeds maximum limit of " <> show maxSizeInBytes <> " bytes (" <> show (fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB) <> "MB)"
  -- Defaults to shadow mode: log what would be rejected without failing onboarding, until the
  -- rejection rate has been measured per merchant. Flip enforceUploadFileTypeCheck to enforce.
  let enforceFileTypeCheck = fromMaybe False transporterConfig.enforceUploadFileTypeCheck
  mbUploadedFileType <- validateUploadedFileType enforceFileTypeCheck imageType imageSizeInBytes fileExtension
  let rcDependentDocuments = [DVC.VehiclePUC, DVC.VehiclePermit, DVC.VehicleInsurance, DVC.VehicleFitnessCertificate, DVC.VehicleNOC, DVC.VehicleBack, DVC.VehicleBackInterior, DVC.VehicleFront, DVC.VehicleFrontInterior, DVC.VehicleRight, DVC.VehicleLeft, DVC.Odometer, DVC.InspectionHub]
  mbRcId <-
    if imageType `elem` rcDependentDocuments
      then case rcNumber of
        Just rcNo -> do
          rc <- QRC.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
          case person.role of
            role | role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS] -> do
              fleetAssoc <- FRCA.findLatestByRCIdAndFleetOwnerId rc.id personId
              when (isNothing fleetAssoc) $ throwError RCNotLinkedWithFleet
              return $ Just rc.id
            _ -> do
              mbAssoc <- QDRCA.findLatestByRCIdAndDriverId rc.id personId
              when (isNothing mbAssoc) $ throwError RCNotLinked
              return $ Just rc.id
        Nothing -> throwError $ RCMandatory (show imageType)
      else return Nothing

  allImages <- Query.findRecentByPersonIdAndImageType personId imageType
  let images = filter ((\txnId -> isNothing txnId || (txnId /= workflowTransactionId)) . (.workflowTransactionId)) allImages
  unless isDashboard $ do
    let onboardingTryLimit = transporterConfig.onboardingTryLimit
    when (length images > onboardingTryLimit * bool 1 2 (imageType == DVC.AadhaarCard || imageType == DVC.DriverLicense)) $ do
      -- not needed now
      driverPhone <- mapM decrypt person.mobileNumber
      notifyErrorToSupport person org.id merchantOpCityId driverPhone org.name ((.failureReason) <$> images)
      throwError (ImageValidationExceedLimit personId.getId)

  -- WorkflowTransactionId is used only in case of hyperverge request
  let mValidatedImage = find ((== Just Documents.VALID) . (.verificationStatus)) images
  case mValidatedImage of
    Just validatedImage
      | imageType /= DVC.DriverLicense,
        imageType /= DVC.ProfilePhoto,
        isJust workflowTransactionId ->
        return $ ImageValidateResponse validatedImage.id
    _ -> do
      when -- This Condition could be merged with the 1st condition above by replacing images with allImages for mValidatedImage.
        ( imageType == DVC.ProfilePhoto
            && any
              ( \img ->
                  img.verificationStatus == Just Documents.VALID
                    && img.workflowTransactionId == workflowTransactionId
              )
              allImages
        )
        $ throwError $ DocumentAlreadyValidated (show imageType)

      -- Driver selfie re-upload lock: allowed only while every face-match-bound doc is absent or INVALID.
      when (imageType == DVC.ProfilePhoto && not isDashboard) $
        enforceSelfieReuploadPolicy person allImages

      imagePath <- createPath personId.getId merchantId.getId imageType (resolveStoredExtension enforceFileTypeCheck mbUploadedFileType fileExtension)
      s3Result <-
        withTryCatch "S3:put:uploadImage" $
          Redis.withLockRedis (imageS3Lock imagePath) 5 $
            S3.put (T.unpack imagePath) image
      case s3Result of
        Left err -> do
          logError $ "Image upload failed to S3:" <> show err
          throwError $ InternalError ("Image upload failed. Please try again")
        Right _ -> pure ()
      imageEntity <- mkImage personId merchantId (Just merchantOpCityId) imagePath imageType mbRcId (convertValidationStatusToVerificationStatus <$> validationStatus) workflowTransactionId sdkFailureReason
      Query.create imageEntity

      -- skipping validation for rc as validation not available in idfy
      let validationFromDocConfigs =
            let mbDocCfg = find (\c -> c.vehicleCategory == fromMaybe CAR vehicleCategory) docConfigs
             in ( maybe True (.isImageValidationRequired) mbDocCfg,
                  mbDocCfg >>= (.markImageValidOnValidationSkip)
                )
      (isImageValidationRequired, markValidOnSkip) <-
        -- A fleet upload carrying an rcNumber is a VEHICLE document; those are governed by
        -- docConfigs like driver uploads. The fleet-owner table only describes the fleet
        -- owner's own identity documents.
        if person.role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS] && isNothing rcNumber
          then do
            --------------- Image validation for fleet (different config table than docConfigs)
            fleetDocConfigs <- listToMaybe <$> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just imageType, role = Nothing}) Nothing
            return
              ( maybe True (.isImageValidationRequired) fleetDocConfigs,
                fleetDocConfigs >>= (.markImageValidOnValidationSkip)
              )
          else return validationFromDocConfigs
      let guardTarget = case mbRcId of
            Just rcId -> SGuard.TargetVehicleById rcId
            Nothing
              | person.role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS] -> SGuard.TargetFleetOwner personId
              | otherwise -> SGuard.TargetDriver personId
          -- Status writes go through the onboarding-action wrapper: entity lock + flag recompute.
          setVerificationStatus status =
            SGuard.withOnboardingAction transporterConfig SGuard.None SGuard.Approve guardTarget $
              Query.updateVerificationStatusOnlyById status imageEntity.id
          markStatusOnValidationSkip =
            setVerificationStatus $
              if markValidOnSkip == Just True
                then Documents.VALID
                else Documents.MANUAL_VERIFICATION_REQUIRED
      if isImageValidationRequired && isNothing validationStatus
        then do
          validationOutput <-
            Verification.validateImage merchantId merchantOpCityId $
              Verification.ValidateImageReq {image, imageType = castImageType imageType, driverId = person.id.getId}
          if validationOutput.validationAvailable
            then do
              checkErrors imageEntity.id imageType validationOutput.detectedImage
              setVerificationStatus Documents.VALID
            else -- the provider cannot validate this document type: a doc that was never checked
            -- must follow the validation-skip semantics, not get stamped VALID
              markStatusOnValidationSkip
        else when (isNothing validationStatus) markStatusOnValidationSkip
      when (imageType == DVC.ProfilePhoto) $
        fork "deferred face match on selfie upload" $ do
          runDeferredFaceMatchOnSelfie person imageEntity.createdAt
          -- Recompute verified/enabled right away: the deferred run may have promoted PENDING docs to VALID.
          void $ SStatus.runRefreshOnboardingFlagsDriver (Just person) Nothing person.id
      when (imageType == DVC.LocalResidenceProof) $
        mapM_
          ( \staleImg -> do
              _ <- withTryCatch "S3:delete:staleLocalResidenceProof" $ S3.delete (T.unpack staleImg.s3Path)
              Query.deleteById staleImg.id
          )
          (filter (\staleImg -> staleImg.id /= imageEntity.id) allImages)
      return $ ImageValidateResponse {imageId = imageEntity.id}
  where
    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just detectedImage) = do
      let outputImageType = detectedImage.imageType
      unless (outputImageType == castImageType imgType) $ throwImageError id_ (ImageInvalidType (show imgType) "")

      unless (fromMaybe False detectedImage.isReadable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) detectedImage.confidence) $
        throwImageError id_ ImageLowQuality

    checkIfGenuineReq :: Id DM.Merchant -> ImageValidateRequest -> Flow ()
    checkIfGenuineReq merchantId request = do
      (txnId, valStatus) <- fromMaybeM (InvalidRequest "Cannot find necessary data for SDK response!!!!") ((,) <$> request.workflowTransactionId <*> request.validationStatus)
      hvResp <- Verification.verifySdkResp merchantId merchantOpCityId (VI.VerifySdkDataReq txnId)
      (respTxnId, respStatus, respUserDetails) <- fromMaybeM (InvalidRequest "Invalid data recieved while validating data.") ((,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= txnId) $ void $ throwValidationError Nothing Nothing Nothing
      when (convertHVStatusToValidationStatus respStatus /= valStatus) $ void $ throwValidationError Nothing Nothing Nothing
      case respUserDetails of
        VI.HVSelfieFlow (VI.SelfieFlow _) -> return ()
        _ -> void $ throwValidationError Nothing Nothing Nothing

validateImage ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) req@ImageValidateRequest {..} = do
  isLocked <- withLockPersonId
  if isLocked
    then do
      finally
        (validateImageHandler isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) req)
        ( do
            Redis.unlockRedis mkLockKey
            logDebug $ "Create Image Lock for PersonId: " <> personId.getId <> " Unlocked"
        )
    else throwError (InvalidRequest "Image upload already in progress, please wait")
  where
    withLockPersonId = do
      isLocked <- Redis.tryLockRedis mkLockKey 45
      return isLocked
    mkLockKey = "CreateImageTransaction:PersonId:-" <> personId.getId <> "-ImageType:" <> show imageType

convertHVStatusToValidationStatus :: Text -> Domain.ValidationStatus
convertHVStatusToValidationStatus status =
  case status of
    "auto_approved" -> Domain.AUTO_APPROVED
    "auto_declined" -> Domain.AUTO_DECLINED
    "needs_review" -> Domain.NEEDS_REVIEW
    "manually_declined" -> Domain.DECLINED
    "manually_approved" -> Domain.APPROVED
    _ -> Domain.DECLINED

throwValidationError :: (EsqDBFlow m r, CacheFlow m r) => Maybe (Id Domain.Image) -> Maybe (Id Domain.Image) -> Maybe Text -> m a
throwValidationError imgId1 imgId2 msg = do
  whenJust (imgId1) Query.deleteById
  whenJust (imgId2) Query.deleteById
  throwError $ InvalidRequest $ fromMaybe "Invalid Data !!!!!" msg

convertValidationStatusToVerificationStatus :: Domain.ValidationStatus -> Documents.VerificationStatus
convertValidationStatusToVerificationStatus = \case
  Domain.AUTO_APPROVED -> Documents.VALID
  Domain.AUTO_DECLINED -> Documents.INVALID
  Domain.APPROVED -> Documents.VALID
  Domain.DECLINED -> Documents.INVALID
  Domain.NEEDS_REVIEW -> Documents.MANUAL_VERIFICATION_REQUIRED

castImageType :: DVC.DocumentType -> Verification.ImageType
castImageType DVC.DriverLicense = Verification.DriverLicense
castImageType DVC.VehicleRegistrationCertificate = Verification.VehicleRegistrationCertificate
castImageType DVC.VehiclePermit = Verification.VehiclePermit
castImageType DVC.VehiclePUC = Verification.VehiclePUC
castImageType DVC.VehicleInsurance = Verification.VehicleInsurance
castImageType DVC.VehicleFitnessCertificate = Verification.VehicleFitnessCertificate
castImageType DVC.VehicleNOC = Verification.VehicleNOC
castImageType _ = Verification.VehicleRegistrationCertificate -- Fix Later

validateImageFile ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateFileRequest ->
  Flow ImageValidateResponse
validateImageFile isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) ImageValidateFileRequest {..} = do
  image' <- L.runIO $ base64Encode <$> BS.readFile image
  validateImage isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) $ ImageValidateRequest image' imageType rcNumber validationStatus workflowTransactionId Nothing Nothing Nothing

mkImage ::
  (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id Person.Person ->
  Id DM.Merchant ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Text ->
  DVC.DocumentType ->
  Maybe (Id DVRC.VehicleRegistrationCertificate) ->
  Maybe Documents.VerificationStatus ->
  Maybe Text ->
  Maybe Text ->
  m Domain.Image
mkImage personId_ merchantId mbMerchantOpCityId s3Path documentType_ mbRcId verificationStatus workflowTransactionId sdkFailureReason = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Image
      { id,
        personId = personId_,
        merchantId,
        s3Path,
        imageType = documentType_,
        verificationStatus = Just $ fromMaybe Documents.PENDING verificationStatus,
        failureReason = ImageNotValid <$> sdkFailureReason,
        rcId = getId <$> mbRcId,
        workflowTransactionId,
        reviewerEmail = Nothing,
        documentExpiry = Nothing,
        createdAt = now,
        updatedAt = now,
        merchantOperatingCityId = mbMerchantOpCityId
      }

getImage :: Id DM.Merchant -> Id Domain.Image -> Flow Text
getImage merchantId imageId = do
  imageMetadata <- Query.findById imageId
  case imageMetadata of
    Just img | img.merchantId == merchantId -> S3.get $ T.unpack img.s3Path
    _ -> pure T.empty

getImageWithAccessCheck :: Id Person.Person -> Id DM.Merchant -> Id Domain.Image -> Flow Text
getImageWithAccessCheck personId merchantId imageId = do
  imageMetadata <- Query.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
  unless (imageMetadata.merchantId == merchantId) $ throwError (ImageAccessDenied imageId.getId)
  unless (imageMetadata.personId == personId) $ do
    mbRequestor <- Person.findById personId
    whenJust mbRequestor $ \requestor -> do
      imageOwner <- Person.findById imageMetadata.personId >>= fromMaybeM (PersonDoesNotExist imageMetadata.personId.getId)
      isValid <- DFOA.isAssociationBetweenTwoPerson requestor imageOwner
      unless isValid $ throwError (ImageAccessDenied imageId.getId)
  S3.get $ T.unpack imageMetadata.s3Path
