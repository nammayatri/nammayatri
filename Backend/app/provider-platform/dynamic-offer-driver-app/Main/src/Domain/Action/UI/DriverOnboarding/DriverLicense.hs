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
import Domain.Types.DocumentVerificationConfig (DocumentVerificationConfig)
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverLicense as Domain
import qualified Domain.Types.DriverPanCard as DPan
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
import SharedLogic.DriverOnboarding
import SharedLogic.Reminder.Helper (createReminder)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as QODC
import qualified Storage.Queries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverLicense as Query
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import qualified Tools.DriverBackgroundVerification as DriverBackgroundVerification
import Tools.Error
import qualified Tools.Ticket as TT
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

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

verifyDL ::
  DPan.VerifiedBy ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverDLReq ->
  Flow DriverDLRes
verifyDL verifyBy mbMerchant (personId, merchantId, merchantOpCityId) req@DriverDLReq {..} = do
  let isDashboard = verifyBy == DPan.DASHBOARD
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyDLHitsCountKey req.driverLicenseNumber) externalServiceRateLimitOptions
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverInfo.driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  documentVerificationConfig <- QODC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId DTO.DriverLicense (fromMaybe CAR req.vehicleCategory) Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show DTO.DriverLicense))
  (nameOnTheCard, dateOfBirth) <-
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
                    void $ VC.compareDateOfBirth extractedDob (Just driverDateOfBirth)
                    return (_nameOnCard, extractedDL.dateOfBirth)
                  Nothing -> throwImageError imageId1 ImageExtractionFailed
          else return (Nothing, Nothing)
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
  let runBody = do
        when (VC.isNameCompareRequired transporterConfig verifyBy) $
          VC.validateDocument merchantId merchantOpCityId person.id nameOnTheCard dateOfBirth Nothing DTO.DriverLicense VC.DriverDocument {panNumber = decryptedPanNumber, aadhaarNumber = decryptedAadhaarNumber, dlNumber = decryptedDlNumber, gstNumber = Nothing}
        mdriverLicense <- Query.findByDLNumber driverLicenseNumber
        case mdriverLicense of
          Just driverLicense -> do
            when (driverLicense.driverId /= personId) $
              if fromMaybe False documentVerificationConfig.allowLicenseTransfer
                then pure ()
                else -- unlinkDLFromDriver driverLicense.driverId
                  throwImageError imageId1 DLAlreadyLinked
            if fromMaybe False documentVerificationConfig.allowLicenseTransfer
              then pure ()
              else unless (driverLicense.licenseExpiry > now) $ throwImageError imageId1 DLAlreadyUpdated
            when (driverLicense.verificationStatus == Documents.VALID && not (fromMaybe False documentVerificationConfig.allowLicenseTransfer)) $ throwError DLAlreadyUpdated
            if documentVerificationConfig.doStrictVerifcation
              then do
                when (driverLicense.verificationStatus == Documents.INVALID) $ throwError DLInvalid
                verifyDLFlow person merchantOpCityId documentVerificationConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard req.vehicleCategory req.requestId sdkTransactionId
              else onVerifyDLHandler person (Just driverLicenseNumber) (Just "2099-12-12") Nothing Nothing Nothing documentVerificationConfig req.imageId1 req.imageId2 nameOnTheCard dateOfIssue req.vehicleCategory
          Nothing -> do
            mDriverDL <- Query.findByDriverId personId
            when (isJust mDriverDL) $ throwImageError imageId1 DriverAlreadyLinked
            if documentVerificationConfig.doStrictVerifcation
              then verifyDLFlow person merchantOpCityId documentVerificationConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard req.vehicleCategory req.requestId sdkTransactionId
              else onVerifyDLHandler person (Just driverLicenseNumber) (Just "2099-12-12") Nothing Nothing (Just . T.pack . show . utctDay $ driverDateOfBirth) documentVerificationConfig req.imageId1 req.imageId2 nameOnTheCard dateOfIssue req.vehicleCategory
  if VC.isNameCompareRequired transporterConfig verifyBy
    then Redis.withWaitOnLockRedisWithExpiry (VC.makeDocumentVerificationLockKey personId.getId) 10 10 runBody
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
          becknIssueId = Nothing
        }

    makeVerifyDLHitsCountKey :: Text -> Text
    makeVerifyDLHitsCountKey dlNumber = "VerifyDL:dlNumberHits:" <> dlNumber <> ":hitsCount"

verifyDLFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> DocumentVerificationConfig -> Text -> UTCTime -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe UTCTime -> Maybe Text -> Maybe VehicleCategory -> Maybe Text -> Maybe Text -> Flow ()
verifyDLFlow person merchantOpCityId documentVerificationConfig dlNumber driverDateOfBirth imageId1 imageId2 dateOfIssue nameOnTheCard mbVehicleCategory mbReqId mbTxnId = do
  now <- getCurrentTime
  encryptedDL <- encrypt dlNumber
  case mbReqId of
    Just reqId -> HVQuery.create =<< mkHyperVergeVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard reqId now Domain.Success encryptedDL mbTxnId
    Nothing -> do
      let imageExtractionValidation =
            if isNothing dateOfIssue && documentVerificationConfig.checkExtraction
              then Domain.Success
              else Domain.Skipped
      verifyRes <-
        Verification.verifyDLAsync person.merchantId merchantOpCityId $
          Verification.VerifyDLAsyncReq {dlNumber, dateOfBirth = driverDateOfBirth, driverId = person.id.getId, returnState = Just True}

      case verifyRes.requestor of
        VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard verifyRes.requestId now imageExtractionValidation encryptedDL
        VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person imageId1 imageId2 mbVehicleCategory driverDateOfBirth dateOfIssue nameOnTheCard verifyRes.requestId now imageExtractionValidation encryptedDL verifyRes.transactionId
        _ -> throwError $ InternalError ("Service provider not configured to return DL verification async responses. Provider Name : " <> (show verifyRes.requestor))

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
        docType = DTO.DriverLicense,
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

onVerifyDL :: VerificationReqRecord -> VerificationIntTypes.DLVerificationOutputInterface -> VT.VerificationService -> Flow AckResponse
onVerifyDL verificationReq output serviceName = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  let key = dlCacheKey person.id
  extractedDlAndOperatingCity <- Redis.safeGet key
  void $ Redis.del key
  case (output.status, verificationReq.issueDateOnDoc, extractedDlAndOperatingCity, verificationReq.driverDateOfBirth) of
    (Just status, Just issueDate, Just (extractedDL, operatingCity), Just dob) | status == "id_not_found" -> dlNotFoundFallback issueDate (extractedDL, operatingCity) dob verificationReq person
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
          documentVerificationConfig <- QODC.findByMerchantOpCityIdAndDocumentTypeAndCategory person.merchantOperatingCityId DTO.DriverLicense (fromMaybe CAR verificationReq.vehicleCategory) Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show DTO.DriverLicense))
          onVerifyDLHandler person output.licenseNumber (output.t_validity_to <|> output.nt_validity_to) output.covs output.driverName output.dob documentVerificationConfig verificationReq.documentImageId1 verificationReq.documentImageId2 verificationReq.nameOnCard Nothing verificationReq.vehicleCategory
          pure Ack

onVerifyDLHandler :: Person.Person -> Maybe Text -> Maybe Text -> Maybe [Idfy.CovDetail] -> Maybe Text -> Maybe Text -> DocumentVerificationConfig -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe Text -> Maybe UTCTime -> Maybe VehicleCategory -> Flow ()
onVerifyDLHandler person dlNumber dlExpiry covDetails name dob documentVerificationConfig imageId1 imageId2 nameOnTheCard dateOfIssue vehicleCategory = do
  now <- getCurrentTime
  id <- generateGUID
  mEncryptedDL <- encrypt `mapM` dlNumber
  let mLicenseExpiry = convertTextToUTC dlExpiry
  let mDriverLicense = createDL person.merchantId documentVerificationConfig person.id covDetails name dob id imageId1 imageId2 nameOnTheCard dateOfIssue vehicleCategory now <$> mEncryptedDL <*> mLicenseExpiry

  whenJust mDriverLicense $ \driverLicense -> do
    Query.upsert driverLicense
    case person.role of
      Person.DRIVER -> do
        DriverInfo.updateDlNumber mEncryptedDL person.id
      _ -> pure ()
    (image1, image2) <- uncurry (liftA2 (,)) $ both (maybe (return Nothing) ImageQuery.findById) (Just imageId1, imageId2)
    when (((image1 >>= (.verificationStatus)) /= Just Documents.VALID) && ((image2 >>= (.verificationStatus)) /= Just Documents.VALID)) $
      mapM_ (maybe (return ()) (ImageQuery.updateVerificationStatusAndFailureReason Documents.VALID (ImageNotValid "verificationStatus updated to VALID by dashboard."))) [Just imageId1, imageId2]
    whenJust driverLicense.driverName $ \name_ ->
      void $ Person.updateName name_ person.id
    -- Create reminders for DL when it's updated
    createReminder
      DVC.DriverLicense
      person.id
      person.merchantId
      person.merchantOperatingCityId
      (Just $ driverLicense.id.getId)
      (Just driverLicense.licenseExpiry)
      Nothing

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
  let classOfVehicles = foldMap (map (.cov)) covDetails
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

dlNotFoundFallback :: UTCTime -> (Text, Text) -> UTCTime -> VerificationReqRecord -> Person.Person -> Flow AckResponse
dlNotFoundFallback issueDate (extractedDL, operatingCity) dob verificationReq person = do
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
  void $ verifyDL DPan.FRONTEND_SDK Nothing (person.id, person.merchantId, person.merchantOperatingCityId) dlreq
  return Ack
