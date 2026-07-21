{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.DriverLicense
  ( DriverDLReq (..),
    DriverDLRes,
    verifyDL,
    onVerifyDL,
    cacheExtractedDl,
    cacheExtractedDlName,
    onVerifyDLHandler,
    convertUTCTimetoDate,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative (liftA2, (<|>))
import qualified Data.Text as T
import Data.Time (nominalDay, utctDay)
import Data.Tuple.Extra (both)
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as VC
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentAuditLog as DAL
import Domain.Types.DocumentVerificationConfig (DocumentVerificationConfig)
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverLicense as Domain
import qualified Domain.Types.DriverPanCard as DPan
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.HyperVergeVerification as Domain
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import Environment
import Kernel.External.Encryption
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.Types as VerificationIntTypes
import Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding.Audit as Audit
import SharedLogic.Reminder.Helper (createReminder)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverLicense as Query
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.MorthVerification as MorthQuery
import qualified Storage.Queries.Person as Person
import qualified Tools.DriverBackgroundVerification as DriverBackgroundVerification
import Tools.Error
import qualified Tools.Ticket as TT
import qualified Tools.Utils as Utils
import qualified Tools.Verification as Verification

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    vehicleCategory :: Maybe VehicleCategory,
    imageId1 :: Id Image.Image,
    imageId2 :: Maybe (Id Image.Image),
    dateOfIssue :: Maybe UTCTime,
    nameOnCard :: Maybe Text,
    nameOnCardFromSdk :: Maybe Text, -- used when frontend sdk is used for extraction
    requestId :: Maybe Text, -- used when frontend sdk is used for extraction
    sdkTransactionId :: Maybe Text, -- used when frontend sdk is used for extraction
    isDLImageValidated :: Maybe Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverDLRes = APISuccess

validateDriverDLReq :: UTCTime -> Validate DriverDLReq
validateDriverDLReq now DriverDLReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber licenseNum,
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t60YearsAgo t18YearsAgo
    ]
  where
    licenseNum = LengthInRange 5 20
    t18YearsAgo = yearsAgo 18
    t60YearsAgo = yearsAgo 80
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

validateDriverDLReqRegexFlow :: UTCTime -> Validate DriverDLReq
validateDriverDLReqRegexFlow now DriverDLReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber (MinLength 1),
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t60YearsAgo t18YearsAgo
    ]
  where
    t18YearsAgo = yearsAgo 18
    t60YearsAgo = yearsAgo 80
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

isDLNumberFormatValid :: DTO.DocumentVerificationConfig -> Text -> Flow Bool
isDLNumberFormatValid documentVerificationConfig normalizedDLNumber =
  validateByRegex "DL" documentVerificationConfig normalizedDLNumber (pure True)

verifyDL ::
  -- | Audit actor. 'Nothing' ⇒ the person verifying is the actor (driver self-verify), resolved from the
  -- fetched person's role. 'Just' ⇒ an external actor (dashboard operator verifying on the driver's behalf).
  Maybe Audit.Requestor ->
  DPan.VerifiedBy ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverDLReq ->
  Flow DriverDLRes
verifyDL mbRequestor verifyBy mbMerchant (personId, merchantId, merchantOpCityId) req@DriverDLReq {..} = do
  let isDashboard = verifyBy == DPan.DASHBOARD
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyDLHitsCountKey req.driverLicenseNumber) externalServiceRateLimitOptions
  now <- getCurrentTime
  documentVerificationConfig <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just DTO.DriverLicense, vehicleCategory = Just (fromMaybe CAR req.vehicleCategory)}) (Just (maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId DTO.DriverLicense (fromMaybe CAR req.vehicleCategory) Nothing)) >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show DTO.DriverLicense))
  let regexRules = getRegexRulesFromDocumentConfig documentVerificationConfig
      hasRegexRules = not (null regexRules)
  if hasRegexRules
    then runRequestValidation (validateDriverDLReqRegexFlow now) req
    else runRequestValidation (validateDriverDLReq now) req
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let requestor = fromMaybe (Audit.driverAppPerson personId (Audit.toActorRole person.role)) mbRequestor
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let normalizedDLNumber = VC.normalizeDocumentNumber driverLicenseNumber
  checkDLFormat <- isDLNumberFormatValid documentVerificationConfig normalizedDLNumber
  unless checkDLFormat $
    throwError (InvalidRequest "DL number format is not valid")
  (extractedNameOnCard, dateOfBirth) <-
    if isJust nameOnCardFromSdk
      then return (nameOnCardFromSdk, Nothing)
      else
        if isNothing dateOfIssue && documentVerificationConfig.checkExtraction && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
          then
            if req.isDLImageValidated == Just True
              then do
                let nameOnTheCard = req.nameOnCard
                let dateOfBirth = Just $ convertUTCTimetoDate req.driverDateOfBirth
                return (nameOnTheCard, dateOfBirth)
              else do
                image1 <- getImage imageId1
                image2 <- getImage `mapM` imageId2
                resp <-
                  Verification.extractDLImage person.merchantId merchantOpCityId $
                    Verification.ExtractImageReq {image1, image2, driverId = person.id.getId}
                case resp.extractedDL of
                  Just extractedDL -> do
                    let extractDLNumber = removeSpaceAndDash <$> extractedDL.dlNumber
                    let dlNumber = removeSpaceAndDash <$> Just driverLicenseNumber
                    let _nameOnCard = extractedDL.nameOnCard
                    -- disable this check for debugging with mock-idfy
                    cacheExtractedDl person.id extractDLNumber operatingCity
                    unless (extractDLNumber == dlNumber) $
                      throwImageError imageId1 $ ImageDocumentNumberMismatch (maybe "null" maskText extractDLNumber) (maybe "null" maskText dlNumber)
                    let extractedDob = VC.parseDateTime =<< extractedDL.dateOfBirth
                    void $ compareDateOfBirth extractedDob (Just driverDateOfBirth)
                    return (_nameOnCard, extractedDL.dateOfBirth)
                  Nothing -> throwImageError imageId1 ImageExtractionFailed
          else return (Nothing, Nothing)
  cachedNameOnCard <- getCachedExtractedDlName person.id
  let nameOnTheCard =
        case extractedNameOnCard of
          Just n | not (T.null n) -> Just n
          _ -> cachedNameOnCard
  decryptedPanNumber <- mapM decrypt driverInfo.panNumber
  decryptedAadhaarNumber <- mapM decrypt driverInfo.aadhaarNumber
  decryptedDlNumber <- mapM decrypt driverInfo.dlNumber
  whenJust transporterConfig.dlNumberVerification $ \dlNumberVerification -> do
    when dlNumberVerification $ do
      fork "driver license verification in safety portal" $ do
        dlVerificationRes <-
          DriverBackgroundVerification.searchAgent person.merchantId merchantOpCityId $
            Agent {dl = Just driverLicenseNumber, voterId = Nothing}
        case dlVerificationRes.suspect of
          [] -> return ()
          res -> do
            let description = encodeToText res
            logInfo $ "Success: " <> show description
            ticket <- TT.createTicket merchantId merchantOpCityId (mkTicket description transporterConfig)
            logInfo $ "Ticket: " <> show ticket
            return ()
  let runDlFaceMatch = do
        -- Reuse the category-aware documentVerificationConfig resolved above (was a category-agnostic lookup).
        dlFaceOutcome <- runDocFaceMatch person documentVerificationConfig imageId1 Nothing (Just driverLicenseNumber)
        when (dlFaceOutcome == FMFail) $ throwError FaceMatchFailed
  let runBody = do
        when (isNameCompareRequired transporterConfig verifyBy) $
          validateDocument merchantId merchantOpCityId person.id nameOnTheCard dateOfBirth Nothing DTO.DriverLicense DriverDocument {panNumber = decryptedPanNumber, aadhaarNumber = decryptedAadhaarNumber, dlNumber = decryptedDlNumber, gstNumber = Nothing}
        mbExistingLicense <- Query.findByDLNumber driverLicenseNumber
        -- let mdriverLicense =
        --       mbExistingLicense >>= \dl ->
        --         if dl.verificationStatus == Documents.INVALID
        --           then Nothing
        --           else Just dl
        case mbExistingLicense of
          Just driverLicense -> do
            logTagInfo "verifyDL" $ "found existing DL record | dlNumber=" <> maskText driverLicenseNumber <> " | id=" <> driverLicense.id.getId <> " | verificationStatus=" <> show driverLicense.verificationStatus
            when (driverLicense.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
              throwError $ DocumentUnderManualReview "DL"
            when (driverLicense.driverId /= personId) $
              if fromMaybe False documentVerificationConfig.allowLicenseTransfer
                then do
                  mDriverDL <- Query.findByDriverIdAndVerificationStatus personId Documents.VALID
                  whenJust mDriverDL $ \_ -> throwImageError imageId1 DriverAlreadyLinked
                else do
                  -- Fleet-aware duplicate check: single query for both drivers' fleet associations
                  allAssocs <- QFDA.findAllByDriverIds [personId, driverLicense.driverId]
                  let existingFleetIds = [assoc.fleetOwnerId | assoc <- allAssocs, assoc.driverId == driverLicense.driverId]
                      targetFleetIds = [assoc.fleetOwnerId | assoc <- allAssocs, assoc.driverId == personId]
                      sharedFleets = filter (`elem` existingFleetIds) targetFleetIds
                  Utils.cleanupUploadedImages ([imageId1] <> maybe [] (\img -> [img]) imageId2) personId
                  unless (null sharedFleets) $ throwError DLAlreadyExistsInFleet
                  when (driverLicense.verificationStatus == Documents.VALID && not (null existingFleetIds)) $
                    throwError DLLinkedToAnotherFleet
                  throwImageError imageId1 DLAlreadyLinked
            if fromMaybe False documentVerificationConfig.allowLicenseTransfer
              then pure ()
              else unless (driverLicense.licenseExpiry > now) $ throwImageError imageId1 DLAlreadyUpdated
            when (driverLicense.verificationStatus == Documents.VALID && not (fromMaybe False documentVerificationConfig.allowLicenseTransfer) && not (fromMaybe False transporterConfig.allowDlReupload)) $ do
              Utils.cleanupUploadedImages ([imageId1] <> maybe [] (\img -> [img]) imageId2) personId
              throwError $ DocumentAlreadyValidated "DL"
            runDlFaceMatch
            if documentVerificationConfig.doStrictVerifcation
              then do
                when (driverLicense.verificationStatus == Documents.INVALID) $
                  if transporterConfig.enableBotFlow == Just True
                    then Query.updateVerificationStatus Documents.PENDING driverLicense.documentImageId1
                    else throwError DLInvalid
                verifyDLFlow requestor person merchantOpCityId documentVerificationConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard req.vehicleCategory req.requestId sdkTransactionId
              else onVerifyDLHandler requestor person (Just driverLicenseNumber) (Just "2099-12-12") Nothing Nothing Nothing documentVerificationConfig req.imageId1 req.imageId2 nameOnTheCard dateOfIssue req.vehicleCategory Nothing
          Nothing -> do
            mDriverDL <- Query.findByDriverIdAndVerificationStatus personId Documents.VALID
            when (isJust mDriverDL) $ do
              Utils.cleanupUploadedImages ([imageId1] <> maybe [] (\img -> [img]) imageId2) personId
              throwImageError imageId1 DriverAlreadyLinked
            runDlFaceMatch
            if documentVerificationConfig.doStrictVerifcation
              then verifyDLFlow requestor person merchantOpCityId documentVerificationConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard req.vehicleCategory req.requestId sdkTransactionId
              else onVerifyDLHandler requestor person (Just driverLicenseNumber) (Just "2099-12-12") Nothing Nothing (Just . T.pack . show . utctDay $ driverDateOfBirth) documentVerificationConfig req.imageId1 req.imageId2 nameOnTheCard dateOfIssue req.vehicleCategory Nothing
  if isNameCompareRequired transporterConfig verifyBy
    then Redis.withWaitOnLockRedisWithExpiry (makeDocumentVerificationLockKey personId.getId) 10 10 runBody
    else runBody
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
      unless (imageMetadata.imageType == DTO.DriverLicense) $
        throwError (ImageInvalidType (show DTO.DriverLicense) "")
      S3.get $ T.unpack imageMetadata.s3Path

    -- unlinkDLFromDriver :: Id Person.Person -> Flow ()
    -- unlinkDLFromDriver pId = do
    --   Query.deleteByDriverId pId
    --   DriverInfo.updateEnabledVerifiedState (cast pId) False (Just False)

    mkTicket description tConfig =
      Ticket.CreateTicketReq
        { category = "BlackListPortal",
          subCategory = Just "Onboarding Dl Verification",
          disposition = tConfig.kaptureDisposition,
          queue = tConfig.kaptureQueue,
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Nothing,
          name = Nothing,
          phoneNo = Nothing,
          personId = personId.getId,
          classification = Ticket.DRIVER,
          rideDescription = Nothing,
          becknIssueId = Nothing,
          ticketContext = Just Ticket.IssueTicket,
          xyneChannelId = Nothing
        }

    makeVerifyDLHitsCountKey :: Text -> Text
    makeVerifyDLHitsCountKey dlNumber = "VerifyDL:dlNumberHits:" <> dlNumber <> ":hitsCount"

verifyDLFlow :: Audit.Requestor -> Person.Person -> Id DMOC.MerchantOperatingCity -> DocumentVerificationConfig -> Text -> UTCTime -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe UTCTime -> Maybe Text -> Maybe VehicleCategory -> Maybe Text -> Maybe Text -> Flow ()
verifyDLFlow requestor person merchantOpCityId documentVerificationConfig dlNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard mbVehicleCategory mbReqId mbTxnId = do
  now <- getCurrentTime
  encryptedDL <- encrypt dlNumber
  -- Async verify: no DL/image row is born yet (only the provider verification-request record). Record a
  -- "verification requested" audit row keyed to the image, attributed to the real actor; the webhook later
  -- writes the result row (externalProvider actor) when the DL is actually created.
  -- eventId = the provider verification requestId, so this request-time row links to the webhook result row.
  let auditVerificationRequested reqId = Audit.auditImageStatusByIdWithEvent requestor (Audit.entityTypeFromRole person.role) person.id.getId "DriverLicense" imageId1 Nothing Documents.PENDING DAL.VERIFICATION_REQUESTED (Just reqId) person.merchantId person.merchantOperatingCityId
  case mbReqId of
    Just reqId -> do
      HVQuery.create =<< mkHyperVergeVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard reqId now Domain.Success encryptedDL mbTxnId
      auditVerificationRequested reqId
    Nothing -> do
      let imageExtractionValidation =
            if isNothing dateOfIssue && documentVerificationConfig.checkExtraction
              then Domain.Success
              else Domain.Skipped
      verifyRes <-
        Verification.verifyDL person.merchantId merchantOpCityId $
          Verification.VerifyDLReq {dlNumber, dateOfBirth = driverDateOfBirth, driverId = person.id.getId, returnState = Just True, applicantMobile = Nothing}

      case verifyRes of
        VerificationIntTypes.AsyncDLResp res -> do
          case res.requestor of
            VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard res.requestId now imageExtractionValidation encryptedDL
            VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard res.requestId now imageExtractionValidation encryptedDL res.transactionId
            _ -> throwError $ InternalError ("Service provider not configured to return DL verification async responses. Provider Name : " <> (show res.requestor))
          auditVerificationRequested res.requestId
        VerificationIntTypes.SyncDLResp resp -> do
          when (resp.requestor == VT.Morth) $ do
            let mbStatus = DocStatus.docStatusEnumToText $ if isJust resp.response.status then DocStatus.DOC_SUCCESS else DocStatus.DOC_FAILED
            morthEntity <- VC.mkMorthVerificationEntity person Nothing DTO.DriverLicense encryptedDL mbStatus dateOfIssue (Just driverDateOfBirth) mbVehicleCategory Nothing Nothing (Just $ show resp.response) now
            MorthQuery.create morthEntity
          onVerifyDLHandler requestor person resp.response.licenseNumber (resp.response.t_validity_to <|> resp.response.nt_validity_to <|> Just "2099-12-12") resp.response.covs resp.response.driverName resp.response.dob documentVerificationConfig imageId1 imageId2 nameOnTheCard dateOfIssue mbVehicleCategory Nothing

mkIdfyVerificationEntity :: Person.Person -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe VehicleCategory -> UTCTime -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime -> Domain.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Flow Domain.IdfyVerification
mkIdfyVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard requestId now imageExtractionValidation encryptedDL = do
  id <- generateGUID
  return $
    Domain.IdfyVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        requestId,
        imageExtractionValidation = imageExtractionValidation,
        documentNumber = encryptedDL,
        issueDateOnDoc = dateOfIssue,
        driverDateOfBirth = Just driverDateOfBirth,
        docType = docTypeToText DTO.DriverLicense,
        status = "pending",
        idfyResponse = Nothing,
        retryCount = Just 0,
        nameOnCard = nameOnTheCard,
        vehicleCategory = mbVehicleCategory,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        airConditioned = Nothing,
        oxygen = Nothing,
        ventilator = Nothing,
        createdAt = now,
        updatedAt = now
      }

mkHyperVergeVerificationEntity :: Person.Person -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe VehicleCategory -> UTCTime -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime -> Domain.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Maybe Text -> Flow Domain.HyperVergeVerification
mkHyperVergeVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard requestId now imageExtractionValidation encryptedDL transactionId = do
  id <- generateGUID
  return $
    Domain.HyperVergeVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        requestId,
        docType = DTO.DriverLicense,
        documentNumber = encryptedDL,
        driverDateOfBirth = Just driverDateOfBirth,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = dateOfIssue,
        status = "pending",
        hypervergeResponse = Nothing,
        vehicleCategory = mbVehicleCategory,
        airConditioned = Nothing,
        oxygen = Nothing,
        ventilator = Nothing,
        retryCount = Just 0,
        nameOnCard = nameOnTheCard,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        ..
      }

onVerifyDL :: Audit.Requestor -> VerificationReqRecord -> VerificationIntTypes.DLVerificationOutputInterface -> VT.VerificationService -> Flow AckResponse
onVerifyDL requestor verificationReq output serviceName = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  let key = dlCacheKey person.id
  extractedDlAndOperatingCity <- Redis.safeGet key
  void $ Redis.del key
  case (output.status, verificationReq.issueDateOnDoc, extractedDlAndOperatingCity, verificationReq.driverDateOfBirth) of
    (Just status, Just issueDate, Just (extractedDL, operatingCity), Just dob) | status == "id_not_found" -> dlNotFoundFallback requestor issueDate (extractedDL, operatingCity) dob verificationReq person
    _ -> linkDl person
  where
    linkDl :: Person.Person -> Flow AckResponse
    linkDl person = do
      if verificationReq.imageExtractionValidation == Domain.Skipped
        && isJust verificationReq.issueDateOnDoc
        && ( (convertUTCTimetoDate <$> verificationReq.issueDateOnDoc)
               /= (convertUTCTimetoDate <$> convertTextToUTC output.dateOfIssue)
           )
        then do
          case serviceName of
            VT.Idfy -> IVQuery.updateExtractValidationStatus Domain.Failed verificationReq.requestId
            VT.HyperVergeRCDL -> HVQuery.updateExtractValidationStatus Domain.Failed verificationReq.requestId
            _ -> throwError $ InternalError ("Unknown Service provider webhook encopuntered in onVerifyDL. Name of provider : " <> show serviceName)
          pure Ack
        else do
          documentVerificationConfig <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, documentType = Just DTO.DriverLicense, vehicleCategory = Just (fromMaybe CAR verificationReq.vehicleCategory)}) (Just (maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory person.merchantOperatingCityId DTO.DriverLicense (fromMaybe CAR verificationReq.vehicleCategory) Nothing)) >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DTO.DriverLicense))
          onVerifyDLHandler requestor person output.licenseNumber (output.t_validity_to <|> output.nt_validity_to) output.covs output.driverName output.dob documentVerificationConfig verificationReq.documentImageId1 verificationReq.documentImageId2 verificationReq.nameOnCard Nothing verificationReq.vehicleCategory (Just verificationReq.requestId)
          pure Ack

onVerifyDLHandler :: Audit.Requestor -> Person.Person -> Maybe Text -> Maybe Text -> Maybe [Idfy.CovDetail] -> Maybe Text -> Maybe Text -> DocumentVerificationConfig -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe Text -> Maybe UTCTime -> Maybe VehicleCategory -> Maybe Text -> Flow ()
onVerifyDLHandler requestor person dlNumber dlExpiry covDetails name dob documentVerificationConfig imageId1 imageId2 nameOnTheCard dateOfIssue vehicleCategory mbEventId = do
  now <- getCurrentTime
  id <- generateGUID
  mEncryptedDL <- encrypt `mapM` dlNumber
  let mLicenseExpiry = convertTextToUTC dlExpiry
  let mDriverLicense = createDL person.merchantId documentVerificationConfig person.id covDetails name dob id imageId1 imageId2 nameOnTheCard dateOfIssue vehicleCategory now <$> mEncryptedDL <*> mLicenseExpiry

  case mDriverLicense of
    Just driverLicense -> do
      (image1, image2) <- uncurry (liftA2 (,)) $ both (maybe (return Nothing) ImageQuery.findById) (Just imageId1, imageId2)
      -- Promote non-terminal images before resolution so MANUAL_VERIFICATION_REQUIRED doesn't read as FMDeferred.
      forM_ [(image1, Just imageId1), (image2, imageId2)] $ \(mbImg, mbImgId) ->
        when ((mbImg >>= (.verificationStatus)) `notElem` [Just Documents.VALID, Just Documents.INVALID]) $
          whenJust mbImgId $ \imgId -> do
            ImageQuery.updateVerificationStatusAndFailureReason Documents.VALID (ImageNotValid "verificationStatus updated to VALID by dashboard.") imgId
            -- Document audit: each DL image flip to VALID alongside the audited DL doc-row write below.
            Audit.auditImageStatusById requestor (Audit.entityTypeFromRole person.role) person.id.getId "DriverLicense" imgId (show <$> (mbImg >>= (.verificationStatus))) Documents.VALID DAL.APPROVED person.merchantId person.merchantOperatingCityId
      -- Record stays PENDING until the face match passes; reuses a recorded result when the match already ran.
      finalStatus <-
        if driverLicense.verificationStatus == Documents.VALID
          then resolveFaceMatchVerificationStatus person documentVerificationConfig imageId1 Nothing dlNumber
          else pure driverLicense.verificationStatus
      mOldDriverLicense <- Audit.fetchForAuditByCity person.merchantOperatingCityId (Query.findByDriverId person.id)
      Query.upsert driverLicense {Domain.verificationStatus = finalStatus}
      let docImageInvalid = any (\mbImg -> (mbImg >>= (.verificationStatus)) == Just Documents.INVALID) [image1, image2]
      when docImageInvalid $ Query.updateVerificationStatus Documents.INVALID imageId1
      -- Document audit: DL verification outcome (result row). Actor is the threaded requestor — the real
      -- initiator on the sync path, or externalProvider/scheduler when reached from a webhook / retry job.
      Audit.auditDocStatusWithEvent requestor (Audit.entityTypeFromRole person.role) person.id.getId "DriverLicense" DAL.DRIVER_LICENSE (Just driverLicense.id.getId) (show . (.verificationStatus) <$> mOldDriverLicense) (Just (show finalStatus)) DAL.STATUS_CHANGED Nothing mbEventId person.merchantId person.merchantOperatingCityId
      case person.role of
        Person.DRIVER -> do
          DriverInfo.updateDlNumber mEncryptedDL person.id
        _ -> pure ()
      case driverLicense.driverName of
        Just name_ -> void $ Person.updateName name_ person.id
        Nothing -> pure ()
      -- Create reminders for DL when it's updated
      createReminder
        DVC.DriverLicense
        person.id
        person.merchantId
        person.merchantOperatingCityId
        (Just $ driverLicense.id.getId)
        (Just driverLicense.licenseExpiry)
        Nothing
      pure ()
    Nothing -> pure ()

dlCacheKey :: Id Person.Person -> Text
dlCacheKey personId =
  "providerPlatform:dlCacheKey:" <> personId.getId

createDL ::
  Id DM.Merchant ->
  DTO.DocumentVerificationConfig ->
  Id Person.Person ->
  Maybe [Idfy.CovDetail] ->
  Maybe Text ->
  Maybe Text ->
  Id Domain.DriverLicense ->
  Id Image.Image ->
  Maybe (Id Image.Image) ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe VehicleCategory ->
  UTCTime ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.DriverLicense
createDL merchantId configs driverId covDetails name dob id imageId1 imageId2 nameOnTheCard dateOfIssue vehicleCategory now edl expiry = do
  let classOfVehicles = maybe [] (map (.cov)) covDetails
  let verificationStatus =
        if configs.doStrictVerifcation
          then validateDLStatus configs expiry classOfVehicles now
          else Documents.MANUAL_VERIFICATION_REQUIRED
  let verifiedName = (\n -> if '*' `T.elem` n then Nothing else Just n) =<< name
  let driverName = verifiedName <|> nameOnTheCard
  Domain.DriverLicense
    { id,
      driverId,
      documentImageId1 = imageId1,
      documentImageId2 = imageId2,
      merchantId = Just merchantId,
      driverDob = convertTextToUTC dob,
      driverName,
      licenseNumber = edl,
      licenseExpiry = expiry,
      classOfVehicles,
      verificationStatus,
      failedRules = [],
      dateOfIssue,
      rejectReason = Nothing,
      vehicleCategory,
      consent = True,
      createdAt = now,
      updatedAt = now,
      consentTimestamp = now
    }

validateDLStatus :: DTO.DocumentVerificationConfig -> UTCTime -> [Text] -> UTCTime -> Documents.VerificationStatus
validateDLStatus configs expiry cov now = do
  case configs.supportedVehicleClasses of
    DTO.DLValidClasses [] -> Documents.INVALID
    DTO.DLValidClasses validCOVs -> do
      let validCOVsCheck = configs.vehicleClassCheckType
      let isCOVValid = foldr' (\x acc -> isValidCOVDL validCOVs validCOVsCheck x || acc) False cov
      if ((not configs.checkExpiry) || now < expiry) && isCOVValid then Documents.VALID else Documents.INVALID
    _ -> Documents.INVALID

isValidCOVDL :: [Text] -> DTO.VehicleClassCheckType -> Text -> Bool
isValidCOVDL validCOVs validCOVsCheck cov =
  checkForClass
  where
    checkForClass = foldr' (\x acc -> classCheckFunction validCOVsCheck (T.toUpper x) (T.toUpper cov) || acc) False validCOVs

cacheExtractedDl :: Id Person.Person -> Maybe Text -> Text -> Flow ()
cacheExtractedDl _ Nothing _ = return ()
cacheExtractedDl personId extractedDL operatingCity = do
  let key = dlCacheKey personId
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  Redis.setExp key (extractedDL, operatingCity) authTokenCacheExpiry

dlNameCacheKey :: Id Person.Person -> Text
dlNameCacheKey personId =
  "providerPlatform:dlNameCacheKey:" <> personId.getId

cacheExtractedDlName :: Id Person.Person -> Maybe Text -> Flow ()
cacheExtractedDlName personId (Just name) | not (T.null name) = do
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  Redis.setExp (dlNameCacheKey personId) name authTokenCacheExpiry
cacheExtractedDlName _ _ = return ()

getCachedExtractedDlName :: Id Person.Person -> Flow (Maybe Text)
getCachedExtractedDlName personId = Redis.safeGet (dlNameCacheKey personId)

dlNotFoundFallback :: Audit.Requestor -> UTCTime -> (Text, Text) -> UTCTime -> VerificationReqRecord -> Person.Person -> Flow AckResponse
dlNotFoundFallback requestor issueDate (extractedDL, operatingCity) dob verificationReq person = do
  let dlreq =
        DriverDLReq
          { driverLicenseNumber = extractedDL,
            operatingCity = operatingCity,
            driverDateOfBirth = dob,
            imageId1 = verificationReq.documentImageId1,
            imageId2 = verificationReq.documentImageId2,
            vehicleCategory = verificationReq.vehicleCategory,
            dateOfIssue = Just issueDate,
            nameOnCardFromSdk = Nothing,
            nameOnCard = verificationReq.nameOnCard,
            requestId = Nothing,
            sdkTransactionId = Nothing,
            isDLImageValidated = Nothing
          }
  void $ verifyDL (Just requestor) DPan.FRONTEND_SDK Nothing (person.id, person.merchantId, person.merchantOperatingCityId) dlreq
  return Ack
