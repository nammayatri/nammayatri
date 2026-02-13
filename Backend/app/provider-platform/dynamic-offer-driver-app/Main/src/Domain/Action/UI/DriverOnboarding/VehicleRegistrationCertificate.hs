{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
  ( DriverVehicleDetails (..),
    DriverRCReq (..),
    DriverRCRes,
    RCStatusReq (..),
    RCValidationReq (..),
    verifyRC,
    parseDateTime,
    onVerifyRC,
    convertUTCTimetoDate,
    deactivateCurrentRC,
    linkRCStatus,
    deleteRC,
    getAllLinkedRCs,
    LinkedRC (..),
    DeleteRCReq (..),
    convertTextToUTC,
    mkIdfyVerificationEntity,
    mkHyperVergeVerificationEntity,
    validateRCResponse,
    VerificationReqRecord (..),
    DriverDocument (..),
    checkPan,
    checkDL,
    checkAadhaar,
    validateDocument,
    compareDateOfBirth,
    makeDocumentVerificationLockKey,
    isNameCompareRequired,
    getDriverDocumentInfo,
    getDocumentImage,
    isNameComparePercentageValid,
    imageS3Lock,
  )
where

import AWS.S3 as S3
import Control.Applicative ((<|>))
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import Data.Text as T hiding (elem, find, length, map, null, zip)
import Data.Time (Day, utctDay)
import Data.Time.Format
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.HyperVergeVerification as Domain
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.RCValidationRules
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow, ServiceFlow, VerificationFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (find)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified SharedLogic.Analytics as Analytics
import SharedLogic.DriverOnboarding
import SharedLogic.Reminder.Helper (createReminder)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as SCO
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as DGQuery
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import Storage.Queries.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as FDA
import qualified Storage.Queries.FleetOwnerInformation as FOI
import qualified Storage.Queries.FleetRCAssociation as FRCAssoc
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Storage.Queries.RCValidationRules
import Storage.Queries.Ride as RQuery
import qualified Storage.Queries.Vehicle as VQuery
import qualified Storage.Queries.VehicleDetails as CQVD
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCExtra
import Tools.Error
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

data DriverVehicleDetails = DriverVehicleDetails
  { vehicleManufacturer :: Text,
    vehicleModel :: Text,
    vehicleColour :: Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    vehicleModelYear :: Maybe Int
  }
  deriving (Generic, ToSchema, Show, ToJSON, FromJSON)

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    udinNumber :: Maybe Text, -- For TTEN certificate validation (TOTO)
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime, -- updatable
    vehicleCategory :: Maybe DVC.VehicleCategory,
    vehicleClass :: Maybe Text,
    airConditioned :: Maybe Bool,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    vehicleDetails :: Maybe DriverVehicleDetails,
    isRCImageValidated :: Maybe Bool -- updatable
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverRCRes = APISuccess

data LinkedRC = LinkedRC
  { rcDetails :: VehicleRegistrationCertificateAPIEntity,
    rcActive :: Bool,
    isFleetRC :: Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype DeleteRCReq = DeleteRCReq
  { rcNo :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data RCStatusReq = RCStatusReq
  { rcNo :: Text,
    isActivate :: Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data RCValidationReq = RCValidationReq
  { fuelType :: Maybe Text,
    vehicleClass :: Maybe Text,
    manufacturer :: Maybe Text,
    model :: Maybe Text,
    mYManufacturing :: Maybe Day
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DriverDocument = DriverDocument
  { panNumber :: Maybe Text,
    aadhaarNumber :: Maybe Text,
    dlNumber :: Maybe Text,
    gstNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

prefixMatchedResult :: Text -> [Text] -> Bool
prefixMatchedResult rcNumber = DL.any (`T.isPrefixOf` rcNumber)

-- Define a common function to handle role-based decryption
getDriverDocumentInfo :: Person.Person -> Flow (Bool, DriverDocument)
getDriverDocumentInfo person = do
  case person.role of
    Person.FLEET_OWNER -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedGstNumber <- mapM decrypt res.gstNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber Nothing decryptedGstNumber)
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedDlNumber <- mapM decrypt res.dlNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber decryptedDlNumber Nothing)

verifyRC ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverRCReq ->
  Bool ->
  Maybe (Id Person.Person) ->
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, _, merchantOpCityId) req bulkUpload mbFleetOwnerId = do
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyRCHitsCountKey req.vehicleRegistrationCertNumber) externalServiceRateLimitOptions

  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  let isTtenCertificate = isJust req.udinNumber
  documentVerificationConfig <- SCO.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId ODC.VehicleRegistrationCertificate (fromMaybe DVC.CAR req.vehicleCategory) Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.VehicleRegistrationCertificate))
  unless isTtenCertificate $ do
    let checkPrefixOfRCNumber = null documentVerificationConfig.rcNumberPrefixList || prefixMatchedResult req.vehicleRegistrationCertNumber documentVerificationConfig.rcNumberPrefixList
    unless checkPrefixOfRCNumber $ throwError (InvalidRequest "RC number prefix is not valid")
    runRequestValidation validateDriverRCReq req

  (blocked, _) <- getDriverDocumentInfo person
  when blocked $ throwError AccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (person.role == Person.DRIVER) $ do
    allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
    unless (length allLinkedRCs < (transporterConfig.rcLimit + (if isDashboard then 1 else 0))) $ throwError (RCLimitReached transporterConfig.rcLimit)
  let mbAirConditioned = maybe req.airConditioned (\category -> if category `elem` [DVC.CAR, DVC.AMBULANCE, DVC.BUS] then req.airConditioned else Just False) req.vehicleCategory
      (mbOxygen, mbVentilator) = maybe (req.oxygen, req.ventilator) (\category -> if category == DVC.AMBULANCE then (req.oxygen, req.ventilator) else (Just False, Just False)) req.vehicleCategory

  unless isTtenCertificate $
    when
      ( isNothing req.vehicleDetails && isNothing req.dateOfRegistration && documentVerificationConfig.checkExtraction
          && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
          && (not bulkUpload)
          && (isNothing req.isRCImageValidated || req.isRCImageValidated == Just False)
      )
      $ do
        image <- getImage req.imageId
        resp <-
          Verification.extractRCImage person.merchantId merchantOpCityId $
            Verification.ExtractImageReq {image1 = image, image2 = Nothing, driverId = person.id.getId}
        case resp.extractedRC of
          Just extractedRC -> do
            let extractRCNumber = removeSpaceAndDash <$> extractedRC.rcNumber
            let rcNumber = removeSpaceAndDash <$> Just req.vehicleRegistrationCertNumber
            -- disable this check for debugging with mock-idfy
            unless (extractRCNumber == rcNumber) $
              throwImageError req.imageId $ ImageDocumentNumberMismatch (maybe "null" maskText extractRCNumber) (maybe "null" maskText rcNumber)
          Nothing -> throwImageError req.imageId ImageExtractionFailed
  whenJust mbFleetOwnerId $ \fleetOwnerId -> do
    Redis.set (makeFleetOwnerKey req.vehicleRegistrationCertNumber) fleetOwnerId.getId
    -- Optionally update existing RC's fleetOwnerId in DB if enabled via transporterConfig
    updateExistingRCFleetOwnerIfEnabled transporterConfig req.vehicleRegistrationCertNumber fleetOwnerId
  encryptedRC <- encrypt req.vehicleRegistrationCertNumber
  let imageExtractionValidation = bool Domain.Skipped Domain.Success (isNothing req.dateOfRegistration && documentVerificationConfig.checkExtraction && not isTtenCertificate)
  Redis.whenWithLockRedis (rcVerificationLockKey req.vehicleRegistrationCertNumber) 60 $ do
    case req.vehicleDetails of
      Just vDetails@DriverVehicleDetails {..} -> do
        vehicleDetails <-
          CQVD.findByMakeAndModelAndYear vehicleManufacturer vehicleModel vehicleModelYear
            |<|>| CQVD.findByMakeAndModelAndYear vehicleManufacturer vehicleModel Nothing
        let mbVehicleVariant =
              (vehicleDetails <&> (.vehicleVariant)) <|> transporterConfig.missingMappingFallbackVariant
        void $ onVerifyRCHandler person (buildRCVerificationResponse vehicleDetails vehicleColour vehicleManufacturer vehicleModel req.vehicleCategory req.vehicleClass) req.vehicleCategory mbAirConditioned req.imageId mbVehicleVariant vehicleDoors vehicleSeatBelts req.dateOfRegistration vDetails.vehicleModelYear mbOxygen mbVentilator Nothing (Just imageExtractionValidation) (Just encryptedRC) req.imageId Nothing Nothing
      Nothing -> verifyRCFlow person merchantOpCityId req.vehicleRegistrationCertNumber req.imageId req.dateOfRegistration req.vehicleCategory mbAirConditioned mbOxygen mbVentilator encryptedRC imageExtractionValidation req.udinNumber
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById imageId_ >>= fromMaybeM (ImageNotFound imageId_.getId)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId_.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_.getId)
      unless (imageMetadata.imageType == ODC.VehicleRegistrationCertificate) $
        throwError (ImageInvalidType (show ODC.VehicleRegistrationCertificate) "")
      Redis.withLockRedisAndReturnValue (imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path

    -- When enabled via transporterConfig, update any existing VehicleRegistrationCertificate
    -- row for this registration number with the new fleetOwnerId.
    updateExistingRCFleetOwnerIfEnabled ::
      DTC.TransporterConfig ->
      Text ->
      Id Person.Person ->
      Flow ()
    updateExistingRCFleetOwnerIfEnabled transporterConfig regNumber fleetOwnerId = do
      when (fromMaybe False transporterConfig.linkFleetToUnVerifiedExistingRC) $ do
        mbExistingRC <- VRCExtra.findLastVehicleRCWrapper regNumber
        whenJust mbExistingRC $ \existingRC -> do
          now <- getCurrentTime
          let updatedRC = existingRC {DVRC.fleetOwnerId = Just fleetOwnerId.getId}
          RCQuery.upsert updatedRC
          mbFleetAssoc <- FRCAssoc.findLinkedByRCIdAndFleetOwnerId fleetOwnerId updatedRC.id now
          when (isNothing mbFleetAssoc) $
            createFleetRCAssociationIfPossible transporterConfig fleetOwnerId updatedRC

    buildRCVerificationResponse vehicleDetails vehicleColour vehicleManufacturer vehicleModel mbVehicleCategory mbVehicleClass =
      Verification.RCVerificationResponse
        { registrationDate = show <$> req.dateOfRegistration,
          registrationNumber = Just req.vehicleRegistrationCertNumber,
          fitnessUpto = Nothing,
          insuranceValidity = Nothing,
          vehicleClass = mbVehicleClass,
          vehicleCategory = show <$> mbVehicleCategory,
          seatingCapacity = Just . String . show <$> (.capacity) =<< vehicleDetails,
          manufacturer = Just vehicleManufacturer,
          permitValidityFrom = Nothing,
          permitValidityUpto = Nothing,
          pucValidityUpto = Nothing,
          manufacturerModel = Just vehicleModel,
          mYManufacturing = Nothing,
          color = Just vehicleColour,
          fuelType = Nothing,
          bodyType = Nothing,
          status = Nothing,
          grossVehicleWeight = Nothing,
          unladdenWeight = Nothing
        }

    makeVerifyRCHitsCountKey :: Text -> Text
    makeVerifyRCHitsCountKey rcNumber = "VerifyRC:rcNumberHits:" <> rcNumber <> ":hitsCount"

imageS3Lock :: Text -> Text
imageS3Lock path = "image-s3-lock-" <> path

makeDocumentVerificationLockKey :: Text -> Text
makeDocumentVerificationLockKey personId = "DocumentVerificationLock:" <> personId

isNameComparePercentageValid :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Verification.NameCompareReq -> Flow Bool
isNameComparePercentageValid merchantId merchantOpCityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case transporterConfig.validNameComparePercentage of
    Just percentage -> do
      resp <- Verification.nameCompare merchantId merchantOpCityId req
      logDebug $ "Name compare percentage response: " <> show resp
      case resp.nameComparedData of
        Just percentageData -> return $ percentageData.match_output.name_match >= percentage
        Nothing -> throwError $ InternalError "Name comparison service returned invalid response"
    Nothing -> return True -- If percentage not configured, assume valid

getDocumentImage :: Id Person.Person -> Text -> ODC.DocumentType -> Flow Text
getDocumentImage personId imageId_ expectedDocType = do
  imageMetadata <- ImageQuery.findById (Id imageId_) >>= fromMaybeM (ImageNotFound imageId_)
  unless (imageMetadata.verificationStatus == Just Documents.VALID) $
    throwError (ImageNotValid imageId_)
  unless (imageMetadata.personId == personId) $
    throwError (ImageNotFound imageId_)
  unless (imageMetadata.imageType == expectedDocType) $
    throwError (ImageInvalidType (show expectedDocType) "")
  Redis.withLockRedisAndReturnValue (imageS3Lock (imageMetadata.s3Path)) 5 $
    S3.get $ T.unpack imageMetadata.s3Path

verifyRCFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> EncryptedHashedField 'AsEncrypted Text -> Domain.ImageExtractionValidation -> Maybe Text -> Flow ()
verifyRCFlow person merchantOpCityId rcNumber imageId dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator encryptedRC imageExtractionValidation mbUdinNumber = do
  now <- getCurrentTime
  verifyRes <-
    Verification.verifyRC person.merchantId
      merchantOpCityId
      Nothing
      Verification.VerifyRCReq {rcNumber = rcNumber, driverId = person.id.getId, token = Nothing, udinNo = mbUdinNumber}
  case verifyRes.verifyRCResp of
    Verification.AsyncResp res -> do
      case res.requestor of
        VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person res.requestId now imageExtractionValidation encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId Nothing Nothing
        VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person res.requestId now imageExtractionValidation encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId Nothing Nothing res.transactionId
        _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> (show res.requestor))
      CQO.setVerificationPriorityList person.id verifyRes.remPriorityList
    Verification.SyncResp res -> do
      void $ onVerifyRC person Nothing res (Just verifyRes.remPriorityList) (Just imageExtractionValidation) (Just encryptedRC) imageId Nothing Nothing Nothing

mkIdfyVerificationEntity :: MonadFlow m => Person.Person -> Text -> UTCTime -> Domain.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> m Domain.IdfyVerification
mkIdfyVerificationEntity person requestId now imageExtractionValidation encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbStatus = do
  id <- generateGUID
  return $
    Domain.IdfyVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId,
        documentImageId2 = Nothing,
        requestId,
        docType = ODC.VehicleRegistrationCertificate,
        documentNumber = encryptedRC,
        driverDateOfBirth = Nothing,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = dateOfRegistration,
        status = fromMaybe "pending" mbStatus,
        idfyResponse = Nothing,
        vehicleCategory = mbVehicleCategory,
        airConditioned = mbAirConditioned,
        oxygen = mbOxygen,
        ventilator = mbVentilator,
        retryCount = Just $ fromMaybe 0 mbRetryCnt,
        nameOnCard = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

mkHyperVergeVerificationEntity :: MonadFlow m => Person.Person -> Text -> UTCTime -> Domain.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> Maybe Text -> m Domain.HyperVergeVerification
mkHyperVergeVerificationEntity person requestId now imageExtractionValidation encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbStatus transactionId = do
  id <- generateGUID
  return $
    Domain.HyperVergeVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId,
        documentImageId2 = Nothing,
        requestId,
        docType = ODC.VehicleRegistrationCertificate,
        documentNumber = encryptedRC,
        driverDateOfBirth = Nothing,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = dateOfRegistration,
        status = fromMaybe "pending" mbStatus,
        hypervergeResponse = Nothing,
        vehicleCategory = mbVehicleCategory,
        airConditioned = mbAirConditioned,
        oxygen = mbOxygen,
        ventilator = mbVentilator,
        retryCount = Just $ fromMaybe 0 mbRetryCnt,
        nameOnCard = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        ..
      }

onVerifyRC :: (VerificationFlow m r, HasField "ttenTokenCacheExpiry" r Seconds, SchedulerFlow r, ServiceFlow m r, HasField "blackListedJobs" r [Text], HasSchemaName SchedulerJobT, EsqDBReplicaFlow m r) => Person.Person -> Maybe VerificationReqRecord -> VT.RCVerificationResponse -> Maybe [VT.VerificationService] -> Maybe Domain.ImageExtractionValidation -> Maybe (EncryptedHashedField 'AsEncrypted Text) -> Id Image.Image -> Maybe Int -> Maybe Text -> Maybe VT.VerificationService -> m AckResponse
onVerifyRC person mbVerificationReq rcVerificationResponse mbRemPriorityList mbImageExtractionValidation mbEncryptedRC imageId mbRetryCnt mbReqStatus mbServiceName = do
  if maybe False (\req -> req.imageExtractionValidation == Domain.Skipped && compareRegistrationDates rcVerificationResponse.registrationDate req.issueDateOnDoc) mbVerificationReq
    then do
      case mbServiceName of
        Just VT.Idfy -> IVQuery.updateExtractValidationStatus Domain.Failed (maybe "" (.requestId) mbVerificationReq)
        Just VT.HyperVergeRCDL -> HVQuery.updateExtractValidationStatus Domain.Failed (maybe "" (.requestId) mbVerificationReq)
        Nothing -> logError "WARNING: Sync API call, this check is redundant still entered in this case!!!!!!"
        _ -> throwError $ InternalError ("Unknown Service provider webhook encountered in onVerifyRC. Name of provider : " <> show mbServiceName)
      return Ack
    else do
      let mbVehicleCategory = mbVerificationReq >>= (.vehicleCategory)
          mbAirConditioned = mbVerificationReq >>= (.airConditioned)
          mbOxygen = mbVerificationReq >>= (.oxygen)
          mbVentilator = mbVerificationReq >>= (.ventilator)
          mbImageExtractionValidation' = mbImageExtractionValidation <|> (mbVerificationReq <&> (.imageExtractionValidation))
          mbEncryptedRC' = mbEncryptedRC <|> (mbVerificationReq <&> (.documentNumber))
      void $ onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned (maybe "" (.documentImageId1) mbVerificationReq) Nothing Nothing Nothing Nothing Nothing mbOxygen mbVentilator mbRemPriorityList mbImageExtractionValidation' mbEncryptedRC' imageId mbRetryCnt mbReqStatus
      return Ack

onVerifyRCHandler :: (VerificationFlow m r, HasField "ttenTokenCacheExpiry" r Seconds, SchedulerFlow r, ServiceFlow m r, HasField "blackListedJobs" r [Text], HasSchemaName SchedulerJobT, EsqDBReplicaFlow m r) => Person.Person -> VT.RCVerificationResponse -> Maybe DVC.VehicleCategory -> Maybe Bool -> Id Image.Image -> Maybe DV.VehicleVariant -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe [VT.VerificationService] -> Maybe Domain.ImageExtractionValidation -> Maybe (EncryptedHashedField 'AsEncrypted Text) -> Id Image.Image -> Maybe Int -> Maybe Text -> m ()
onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned mbDocumentImageId mbVehicleVariant mbVehicleDoors mbVehicleSeatBelts mbDateOfRegistration mbVehicleModelYear mbOxygen mbVentilator mbRemPriorityList mbImageExtractionValidation mbEncryptedRC imageId mbRetryCnt mbReqStatus' = do
  let mbGrossVehicleWeight = rcVerificationResponse.grossVehicleWeight
      mbUnladdenWeight = rcVerificationResponse.unladdenWeight
  mbFleetOwnerId <- maybe (pure Nothing) (Redis.safeGet . makeFleetOwnerKey) rcVerificationResponse.registrationNumber
  now <- getCurrentTime
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  rcValidationRules <- findByCityId person.merchantOperatingCityId
  let rcValidationReq = RCValidationReq {mYManufacturing = convertTextToDay (rcVerificationResponse.mYManufacturing <> Just "-01"), fuelType = rcVerificationResponse.fuelType, vehicleClass = rcVerificationResponse.vehicleClass, manufacturer = rcVerificationResponse.manufacturer, model = rcVerificationResponse.manufacturerModel}
  failures <- case rcValidationRules of
    Nothing -> pure []
    Just rules -> validateRCResponse rcValidationReq rules
  let mbReqStatus = if null failures then mbReqStatus' else Just "failed"
      isExcludedVehicleCategoryFromVerification = maybe False (`elem` (map show $ fromMaybe [] transporterConfig.vehicleCategoryExcludedFromVerification)) rcVerificationResponse.vehicleCategory
      vehicleCategory' = if isExcludedVehicleCategoryFromVerification then mapTextToVehicle rcVerificationResponse.vehicleCategory else mbVehicleCategory
      rcInput = createRCInput vehicleCategory' mbFleetOwnerId mbDocumentImageId mbDateOfRegistration mbVehicleModelYear mbGrossVehicleWeight mbUnladdenWeight
      checks =
        if transporterConfig.rcExpiryChecks == Just True
          then
            [ ("Fitness Certificate", convertTextToUTC rcVerificationResponse.fitnessUpto),
              ("Insurance", convertTextToUTC rcVerificationResponse.insuranceValidity),
              ("Permit", convertTextToUTC rcVerificationResponse.permitValidityUpto),
              ("PUC", convertTextToUTC rcVerificationResponse.pucValidityUpto)
            ]
          else []
      expiryFailures =
        mapMaybe
          ( \(field, expiry) ->
              if maybe False (< now) expiry
                then Just (T.replace " " "" field <> "Expired")
                else Nothing
          )
          checks
      allFailures = failures <> expiryFailures
  mVehicleRC <- do
    case mbVehicleVariant of
      Just vehicleVariant ->
        maybeM
          (return Nothing)
          ((Just <$>) . createVehicleRC person.merchantId person.merchantOperatingCityId rcInput vehicleVariant allFailures)
          (encrypt `mapM` rcVerificationResponse.registrationNumber)
      Nothing -> buildRC person.merchantId person.merchantOperatingCityId rcInput allFailures
  if isNothing mbVehicleVariant && mbRemPriorityList /= Just [] && isJust mbRemPriorityList && ((mVehicleRC <&> (.verificationStatus)) == Just Documents.MANUAL_VERIFICATION_REQUIRED || join (mVehicleRC <&> (.reviewRequired)) == Just True)
    then do
      flip (maybe (logError "imageExtrationValidation flag or encryptedRC or registrationNumber is null in onVerifyRCHandler. Not proceeding with alternate service providers !!!!!!!!!" >> initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures)) ((,,,) <$> mbImageExtractionValidation <*> mbEncryptedRC <*> mbRemPriorityList <*> rcVerificationResponse.registrationNumber) $
        \(imageExtractionValidation, encryptedRC, remPriorityList, rcNum) -> do
          logDebug $ "Calling verify RC with another provider as current provider resulted in MANUAL_VERIFICATION_REQUIRED. Remaining providers in priorityList : " <> show remPriorityList
          resVerifyRes <- withTryCatch "verifyRC:onVerifyRCHandler" $ Verification.verifyRC person.merchantId person.merchantOperatingCityId (Just remPriorityList) (Verification.VerifyRCReq {rcNumber = rcNum, driverId = person.id.getId, token = Nothing, udinNo = Nothing})
          case resVerifyRes of
            Left _ -> initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures
            Right verifyRes -> do
              case verifyRes.verifyRCResp of
                Verification.AsyncResp res -> do
                  case res.requestor of
                    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person res.requestId now imageExtractionValidation encryptedRC mbDateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbReqStatus
                    VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person res.requestId now imageExtractionValidation encryptedRC mbDateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbReqStatus res.transactionId
                    _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> T.pack (show res.requestor))
                  CQO.setVerificationPriorityList person.id verifyRes.remPriorityList
                Verification.SyncResp resp -> do
                  onVerifyRCHandler person resp mbVehicleCategory mbAirConditioned mbDocumentImageId mbVehicleVariant mbVehicleDoors mbVehicleSeatBelts mbDateOfRegistration mbVehicleModelYear mbOxygen mbVentilator (Just verifyRes.remPriorityList) mbImageExtractionValidation mbEncryptedRC imageId mbRetryCnt mbReqStatus
    else initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures
  where
    createRCInput :: Maybe DVC.VehicleCategory -> Maybe Text -> Id Image.Image -> Maybe UTCTime -> Maybe Int -> Maybe Float -> Maybe Float -> CreateRCInput
    createRCInput vehicleCategory fleetOwnerId documentImageId' dateOfRegistration vehicleModelYear mbGrossVehicleWeight mbUnladdenWeight =
      CreateRCInput
        { registrationNumber = rcVerificationResponse.registrationNumber,
          fitnessUpto = convertTextToUTC rcVerificationResponse.fitnessUpto,
          fleetOwnerId,
          vehicleCategory,
          airConditioned = mbAirConditioned,
          oxygen = mbOxygen,
          ventilator = mbVentilator,
          documentImageId = documentImageId',
          vehicleClass = rcVerificationResponse.vehicleClass,
          vehicleClassCategory = rcVerificationResponse.vehicleCategory,
          insuranceValidity = convertTextToUTC rcVerificationResponse.insuranceValidity,
          seatingCapacity = (readMaybe . T.unpack) =<< readFromJson =<< rcVerificationResponse.seatingCapacity,
          permitValidityUpto = convertTextToUTC rcVerificationResponse.permitValidityUpto,
          pucValidityUpto = convertTextToUTC rcVerificationResponse.pucValidityUpto,
          manufacturer = rcVerificationResponse.manufacturer,
          mYManufacturing = convertTextToDay (rcVerificationResponse.mYManufacturing <> Just "-01"), -- Appending date because we receive mYManufacturing in yyyy-mm format
          manufacturerModel = rcVerificationResponse.manufacturerModel,
          bodyType = rcVerificationResponse.bodyType,
          fuelType = rcVerificationResponse.fuelType,
          dateOfRegistration,
          vehicleModelYear,
          color = rcVerificationResponse.color,
          grossVehicleWeight = mbGrossVehicleWeight,
          unladdenWeight = mbUnladdenWeight
        }

    readFromJson (String val) = Just val
    readFromJson (Number val) = Just $ T.pack $ show (floor val :: Int)
    readFromJson _ = Nothing

    createVehicleRC :: MonadFlow m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> CreateRCInput -> DV.VehicleVariant -> [Text] -> EncryptedHashedField 'AsEncrypted Text -> m DVRC.VehicleRegistrationCertificate
    createVehicleRC merchantId merchantOperatingCityId input vehicleVariant failedRules certificateNumber = do
      now <- getCurrentTime
      id <- generateGUID
      let updatedVehicleVariant = case input.vehicleCategory of
            Just DVC.TRUCK -> DV.getTruckVehicleVariant input.grossVehicleWeight input.unladdenWeight vehicleVariant
            Just DVC.TOTO -> DV.E_RICKSHAW
            _ -> vehicleVariant
      logInfo $ "createVehicleRC: Creating RC with verificationStatus=MANUAL_VERIFICATION_REQUIRED, vehicleVariant=" <> show vehicleVariant <> ", failedRules=" <> show failedRules <> ", registrationNumber=" <> show input.registrationNumber
      return $
        DVRC.VehicleRegistrationCertificate
          { id,
            documentImageId = input.documentImageId,
            certificateNumber,
            fitnessExpiry = fromMaybe (addUTCTime (secondsToNominalDiffTime 788400000) now) input.fitnessUpto, -- TODO :: Please fix me, if my usage is critical. I am hardcoded for next 50 years.
            permitExpiry = input.permitValidityUpto,
            pucExpiry = input.pucValidityUpto,
            vehicleClass = input.vehicleClass,
            vehicleVariant = Just updatedVehicleVariant,
            vehicleManufacturer = input.manufacturer <|> input.manufacturerModel,
            vehicleCapacity = input.seatingCapacity,
            vehicleModel = input.manufacturerModel,
            vehicleColor = input.color,
            manufacturerModel = input.manufacturerModel,
            vehicleEnergyType = input.fuelType,
            reviewedAt = Nothing,
            reviewRequired = Nothing,
            insuranceValidity = input.insuranceValidity,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            fleetOwnerId = input.fleetOwnerId,
            merchantId = Just merchantId,
            mYManufacturing = input.mYManufacturing,
            merchantOperatingCityId = Just merchantOperatingCityId,
            userPassedVehicleCategory = input.vehicleCategory,
            airConditioned = input.airConditioned,
            oxygen = input.oxygen,
            ventilator = input.ventilator,
            luggageCapacity = Nothing,
            vehicleRating = Nothing,
            failedRules = failedRules,
            dateOfRegistration = input.dateOfRegistration,
            vehicleModelYear = input.vehicleModelYear,
            vehicleDoors = mbVehicleDoors,
            vehicleSeatBelts = mbVehicleSeatBelts,
            rejectReason = Nothing,
            createdAt = now,
            unencryptedCertificateNumber = input.registrationNumber,
            approved = Just False,
            updatedAt = now,
            vehicleImageId = Nothing
          }
    initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures = do
      case mVehicleRC of
        Just vehicleRC
          | vehicleRC.verificationStatus == Documents.INVALID ->
            throwError $
              InvalidRequest $
                "No valid mapping found for (vehicleClass: "
                  <> fromMaybe "null" rcVerificationResponse.vehicleClass
                  <> ", vehicleClassCategory: "
                  <> maybe "null" (T.pack . show) rcVerificationResponse.vehicleCategory
                  <> ", manufacturer: "
                  <> fromMaybe "null" rcVerificationResponse.manufacturer
                  <> ", model: "
                  <> fromMaybe "null" rcVerificationResponse.manufacturerModel
                  <> ")"
        Just vehicleRC -> do
          logInfo $ "initiateRCCreation: Upserting RC with verificationStatus=" <> show vehicleRC.verificationStatus <> ", failedRules=" <> show vehicleRC.failedRules <> ", registrationNumber=" <> show vehicleRC.unencryptedCertificateNumber <> ", vehicleVariant=" <> show vehicleRC.vehicleVariant <> ", vehicleClass=" <> show vehicleRC.vehicleClass
          -- upsert vehicleRC
          RCQuery.upsert vehicleRC
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" rcVerificationResponse.registrationNumber))
          -- Create reminders for all expiry dates stored in RC when it's created/updated
          -- Fitness expiry (VEHICLE_REGISTRATION_CERTIFICATE) - always present
          createReminder
            ODC.VehicleRegistrationCertificate
            person.id
            person.merchantId
            person.merchantOperatingCityId
            (Just $ rc.id.getId)
            (Just rc.fitnessExpiry)
            Nothing
          -- PUC expiry
          whenJust rc.pucExpiry $ \pucExpiry -> do
            createReminder
              ODC.VehiclePUC
              person.id
              person.merchantId
              person.merchantOperatingCityId
              (Just $ rc.id.getId)
              (Just pucExpiry)
              Nothing
          -- Permit expiry
          whenJust rc.permitExpiry $ \permitExpiry -> do
            createReminder
              ODC.VehiclePermit
              person.id
              person.merchantId
              person.merchantOperatingCityId
              (Just $ rc.id.getId)
              (Just permitExpiry)
              Nothing
          -- Insurance validity
          whenJust rc.insuranceValidity $ \insuranceValidity -> do
            createReminder
              ODC.VehicleInsurance
              person.id
              person.merchantId
              person.merchantOperatingCityId
              (Just $ rc.id.getId)
              (Just insuranceValidity)
              Nothing
          case person.role of
            Person.FLEET_OWNER -> do
              mbFleetAssoc <- FRCAssoc.findLinkedByRCIdAndFleetOwnerId person.id rc.id now
              when (isNothing mbFleetAssoc) $ do
                createFleetRCAssociationIfPossible transporterConfig person.id rc
            _ -> do
              -- linking to driver
              whenJust mbFleetOwnerId $ \fleetOwnerId -> do
                mbFleetAssoc <- FRCAssoc.findLinkedByRCIdAndFleetOwnerId (Id fleetOwnerId) rc.id now
                when (isNothing mbFleetAssoc) $ do
                  createFleetRCAssociationIfPossible transporterConfig (Id fleetOwnerId :: Id Person.Person) rc
              mbAssoc <- DAQuery.findLinkedByRCIdAndDriverId person.id rc.id now
              when (isNothing mbAssoc) $ do
                createDriverRCAssociationIfPossible transporterConfig person.id rc
              -- update vehicle details too if exists
              mbVehicle <- VQuery.findByRegistrationNo =<< decrypt rc.certificateNumber
              whenJust mbVehicle $ \vehicle -> do
                when (rc.verificationStatus == Documents.VALID && isJust rc.vehicleVariant && null allFailures) $ do
                  driverInfo <- DIQuery.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
                  driver <- Person.findById vehicle.driverId >>= fromMaybeM (PersonNotFound vehicle.driverId.getId)
                  -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
                  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
                  let updatedVehicle = makeFullVehicleFromRC vehicleServiceTiers driverInfo driver person.merchantId vehicle.registrationNo rc person.merchantOperatingCityId now Nothing
                  VQuery.upsert updatedVehicle
              whenJust rcVerificationResponse.registrationNumber $ \num -> Redis.del $ makeFleetOwnerKey num
        Nothing -> pure ()

    mapTextToVehicle :: Maybe Text -> Maybe DVC.VehicleCategory
    mapTextToVehicle = \case
      Just "AUTO_RICKSHAW" -> Just DVC.AUTO_CATEGORY
      Just "AUTO_PLUS" -> Just DVC.AUTO_CATEGORY
      Just "CAB" -> Just DVC.CAR
      Just "TWO_WHEELER" -> Just DVC.MOTORCYCLE
      Just "MOTORCYCLE" -> Just DVC.MOTORCYCLE
      Just "AMBULANCE" -> Just DVC.AMBULANCE
      Just "TRUCK" -> Just DVC.TRUCK
      Just "AUTO_LITE" -> Just DVC.AUTO_CATEGORY
      Just "PINK_AUTO" -> Just DVC.AUTO_CATEGORY
      Just "BUS" -> Just DVC.BUS
      Just "TOTO" -> Just DVC.TOTO
      _ -> Nothing

validateRCResponse :: MonadFlow m => RCValidationReq -> RCValidationRules -> m [Text]
validateRCResponse rc rule = do
  now <- getCurrentTime
  let fuelValid = maybe True (\ft -> isNothing rule.fuelType || Kernel.Prelude.any (\ftRule -> ftRule `isInfixOf` ft) (map T.toLower $ fromMaybe [] rule.fuelType)) (T.toLower <$> rc.fuelType)
      vehicleClassValid = maybe True (\vc -> isNothing rule.vehicleClass || Kernel.Prelude.any (\vcRule -> vcRule `isInfixOf` vc) (map T.toLower $ fromMaybe [] rule.vehicleClass)) (T.toLower <$> rc.vehicleClass)

      manufacturerValid = case rule.vehicleOEM of
        Nothing -> True
        Just oems ->
          let lowerOems = map T.toLower oems
              matches val = Kernel.Prelude.any (`T.isInfixOf` T.toLower val) lowerOems
           in (maybe False matches rc.manufacturer)
                || (maybe False matches rc.model)

      vehicleAge = getVehicleAge rc.mYManufacturing now
      vehicleAgeValid = ((.getMonths) <$> vehicleAge) <= rule.maxVehicleAge
      failures =
        catMaybes
          [ if not fuelValid then Just ("InvalidFuelType:" <> fromMaybe "" rc.fuelType) else Nothing,
            if not vehicleClassValid then Just ("InvalidVehicleClass:" <> fromMaybe "" rc.vehicleClass) else Nothing,
            if not manufacturerValid then Just ("InvalidOEM:" <> fromMaybe "" (rc.manufacturer <|> rc.model)) else Nothing,
            if not vehicleAgeValid then Just ("InvalidManufacturingYear:" <> maybe "" (T.take 7 . T.pack . show) rc.mYManufacturing) else Nothing
          ]
  return failures

compareRegistrationDates :: Maybe Text -> Maybe UTCTime -> Bool
compareRegistrationDates actualDate providedDate =
  isJust providedDate
    && ((convertUTCTimetoDate <$> providedDate) /= (convertUTCTimetoDate <$> convertTextToUTC actualDate))

linkRCStatus :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> RCStatusReq -> Flow APISuccess
linkRCStatus (driverId, merchantId, merchantOpCityId) req@RCStatusReq {..} = runInMasterDbAndRedis $ do
  driverInfo <- DIQuery.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  unless (rc.verificationStatus == Documents.VALID) $ do
    DAQuery.updateRcErrorMessage driverId rc.id "Can't perform activate/inactivate operations on invalid RC!"
    throwError (InvalidRequest "Can't perform activate/inactivate operations on invalid RC!")
  now <- getCurrentTime
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  if req.isActivate
    then do
      validated <- validateRCActivation driverId transporterConfig rc
      when validated $ activateRC driverInfo merchantId merchantOpCityId transporterConfig now rc
    else do
      deactivateRC transporterConfig rc driverId
  return Success

deactivateRC :: DTC.TransporterConfig -> Domain.VehicleRegistrationCertificate -> Id Person.Person -> Flow ()
deactivateRC transporterConfig rc driverId = do
  activeAssociation <- DAQuery.findActiveAssociationByRC rc.id True >>= fromMaybeM ActiveRCNotFound
  unless (activeAssociation.driverId == driverId) $ do
    DAQuery.updateRcErrorMessage driverId rc.id "Driver can't deactivate RC which is not active with them"
    throwError (InvalidRequest "Driver can't deactivate RC which is not active with them")
  removeVehicle driverId
  DAQuery.deactivateRCForDriver False driverId rc.id
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ Analytics.decrementFleetOwnerAnalyticsActiveVehicleCount rc.fleetOwnerId driverId
  return ()

removeVehicle :: Id Person.Person -> Flow ()
removeVehicle driverId = do
  isOnRide <- DIQuery.findByDriverIdActiveRide (cast driverId)
  when (isJust isOnRide) $ throwError RCVehicleOnRide
  VQuery.deleteById driverId -- delete the vehicle entry too for the driver

validateRCActivation :: Id Person.Person -> DTC.TransporterConfig -> Domain.VehicleRegistrationCertificate -> Flow Bool
validateRCActivation driverId transporterConfig rc = do
  now <- getCurrentTime
  mAssoc <- DAQuery.findLinkedByRCIdAndDriverId driverId rc.id now
  case mAssoc of
    Just _ -> return ()
    Nothing -> do
      unless (transporterConfig.allowDriverToUseFleetRcs == Just True) $ do
        DAQuery.updateRcErrorMessage driverId rc.id "RC is not associated with the driver"
        throwError (InvalidRequest "RC is not associated with the driver")
      fleetDriverAssociation <- FDA.findByDriverId driverId True >>= fromMaybeM (InvalidRequest "RC is not linked with the driver.")
      let fleetOwnerId = fleetDriverAssociation.fleetOwnerId
      unless (rc.fleetOwnerId == Just fleetOwnerId) $ do
        DAQuery.updateRcErrorMessage driverId rc.id "RC does not belong to you or your fleet."
        throwError (InvalidRequest "RC does not belong to you or your fleet.")
      createDriverRCAssociationIfPossible transporterConfig driverId rc

  -- check if rc is already active to other driver
  mActiveAssociation <- DAQuery.findActiveAssociationByRC rc.id True
  case mActiveAssociation of
    Just activeAssociation -> do
      if activeAssociation.driverId == driverId
        then return False
        else do
          deactivateIfWeCanDeactivate activeAssociation.driverId now (deactivateRC transporterConfig rc)
          return True
    Nothing -> do
      -- check if vehicle of that rc number is already with other driver
      mVehicle <- VQuery.findByRegistrationNo =<< decrypt rc.certificateNumber
      case mVehicle of
        Just vehicle -> do
          if vehicle.driverId /= driverId
            then deactivateIfWeCanDeactivate vehicle.driverId now removeVehicle
            else removeVehicle driverId
        Nothing -> return ()
      return True
  where
    deactivateIfWeCanDeactivate :: Id Person.Person -> UTCTime -> (Id Person.Person -> Flow ()) -> Flow ()
    deactivateIfWeCanDeactivate oldDriverId now deactivateFunc = do
      driverInfo <- DIQuery.findById oldDriverId >>= fromMaybeM (PersonNotFound oldDriverId.getId)
      let canUnlinkWhenOffline = transporterConfig.allowRcUnlinkWhenDriverOffline == Just True && driverInfo.mode == Just DCommon.OFFLINE
      mLastRideAssigned <- RQuery.findLastRideAssigned oldDriverId
      case mLastRideAssigned of
        Just lastRide -> do
          if (nominalDiffTimeToSeconds (diffUTCTime now lastRide.createdAt) > transporterConfig.automaticRCActivationCutOff || canUnlinkWhenOffline) && driverInfo.onRide == False
            then deactivateFunc oldDriverId
            else do
              DAQuery.updateRcErrorMessage oldDriverId rc.id (show RCActiveOnOtherAccount)
              throwError RCActiveOnOtherAccount
        Nothing -> do
          -- if driver didn't take any ride yet
          person <- Person.findById oldDriverId >>= fromMaybeM (PersonNotFound oldDriverId.getId)
          if (nominalDiffTimeToSeconds (diffUTCTime now person.createdAt) > transporterConfig.automaticRCActivationCutOff || canUnlinkWhenOffline) && driverInfo.onRide == False
            then deactivateFunc oldDriverId
            else do
              DAQuery.updateRcErrorMessage oldDriverId rc.id (show RCActiveOnOtherAccount)
              throwError RCActiveOnOtherAccount

activateRC :: DI.DriverInformation -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTC.TransporterConfig -> UTCTime -> Domain.VehicleRegistrationCertificate -> Flow ()
activateRC driverInfo merchantId merchantOpCityId transporterConfig now rc = do
  when (transporterConfig.requiresOnboardingInspection == Just True) $ do
    unless driverInfo.enabled $ do
      DAQuery.updateRcErrorMessage driverInfo.driverId rc.id "Driver is not enabled"
      throwError (InvalidRequest "Driver is not enabled")
    unless (fromMaybe False rc.approved) $ do
      DAQuery.updateRcErrorMessage driverInfo.driverId rc.id "Vehicle is not approved"
      throwError (InvalidRequest "Vehicle is not approved")
  deactivateCurrentRC transporterConfig driverInfo.driverId
  addVehicleToDriver
  DAQuery.activateRCForDriver driverInfo.driverId rc.id now
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ Analytics.incrementFleetOwnerAnalyticsActiveVehicleCount rc.fleetOwnerId driverInfo.driverId
  return ()
  where
    addVehicleToDriver = do
      rcNumber <- decrypt rc.certificateNumber
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == DV.SUV) $
          DIQuery.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi driverInfo.driverId
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
      person <- Person.findById driverInfo.driverId >>= fromMaybeM (PersonNotFound driverInfo.driverId.getId)
      -- driverStats <- runInReplica $ QDriverStats.findById driverInfo.driverId >>= fromMaybeM DriverInfoNotFound
      let vehicle = makeFullVehicleFromRC cityVehicleServiceTiers driverInfo person merchantId rcNumber rc merchantOpCityId now Nothing
      VQuery.create vehicle

deactivateCurrentRC :: DTC.TransporterConfig -> Id Person.Person -> Flow ()
deactivateCurrentRC transporterConfig driverId = do
  mActiveAssociation <- DAQuery.findActiveAssociationByDriver driverId True
  case mActiveAssociation of
    Just association -> do
      rc <- RCQuery.findById association.rcId >>= fromMaybeM (RCNotFound "")
      deactivateRC transporterConfig rc driverId -- call deativate RC flow
    Nothing -> do
      removeVehicle driverId
      return ()

deleteRC :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DeleteRCReq -> Bool -> Flow APISuccess
deleteRC (driverId, _, merchantOpCityId) DeleteRCReq {..} isOldFlow = do
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  mAssoc <- DAQuery.findActiveAssociationByRC rc.id True
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case (mAssoc, isOldFlow) of
    (Just assoc, False) -> do
      when (assoc.driverId == driverId) $ do
        DAQuery.updateRcErrorMessage driverId rc.id "Deactivate RC first to delete!"
        throwError (InvalidRequest "Deactivate RC first to delete!")
    (Just _, True) -> deactivateRC transporterConfig rc driverId
    (_, _) -> return ()
  DAQuery.endAssociationForRC driverId rc.id
  return Success

getAllLinkedRCs :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow [LinkedRC]
getAllLinkedRCs (driverId, _, _) = do
  driverRCs <- DAQuery.findAllLinkedByDriverId driverId
  rcs <- RCQuery.findAllById (map (.rcId) driverRCs)
  let activeRcs = buildRcHM driverRCs
  mapM (getCombinedRcData activeRcs) rcs
  where
    getCombinedRcData activeRcs rc = do
      rcNo <- decrypt rc.certificateNumber
      return $
        LinkedRC
          { rcActive = fromMaybe False $ HM.lookup rc.id.getId activeRcs <&> (.isRcActive),
            rcDetails = makeRCAPIEntity rc rcNo,
            isFleetRC = isJust rc.fleetOwnerId
          }

rcVerificationLockKey :: Text -> Text
rcVerificationLockKey rcNumber = "VehicleRC::RCNumber-" <> rcNumber

makeFleetOwnerKey :: Text -> Text
makeFleetOwnerKey vehicleNo = "FleetOwnerId:PersonId-" <> removeSpaceAndDash vehicleNo

parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

-- Common function for name comparison
compareNames :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Id Person.Person -> Flow Bool
compareNames merchantId merchantOpCityId mbExtractedName mbVerifiedName personId =
  case (mbExtractedName, mbVerifiedName) of
    (Just extractedName, Just verifiedName) -> do
      let nameCompareReq =
            Verification.NameCompareReq
              { extractedName = extractedName,
                verifiedName = verifiedName,
                percentage = Just True,
                driverId = personId.getId
              }
      isNameValid <- isNameComparePercentageValid merchantId merchantOpCityId nameCompareReq
      unless isNameValid $ throwError (MismatchDataError "Name match failed with previously uploaded docs")
      return True
    _ -> do
      logInfo "Name comparison checks not executed."
      return False

compareDateOfBirth :: Maybe UTCTime -> Maybe UTCTime -> Flow Bool
compareDateOfBirth mbExtractedValue mbVerifiedValue = do
  case (mbExtractedValue, mbVerifiedValue) of
    (Just extractedValue, Just verifiedValue) -> do
      let extractedDay = utctDay extractedValue
          verifiedDay = utctDay verifiedValue
      unless (extractedDay == verifiedDay) $ throwError (MismatchDataError $ "Date of birth mismatch: " <> show extractedDay <> " " <> show verifiedDay)
      return True
    _ -> do
      logInfo "Date of birth checks not executed."
      return False

checkPan :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkPan merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  mdriverPanInformation <- DPQuery.findByDriverId personId
  case mdriverPanInformation of
    Just panData -> do
      if verifyingDocumentType `elem` [ODC.AadhaarCard, ODC.DriverLicense]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue panData.driverNameOnGovtDB mbDateOfBirthValue panData.driverDob personId
        else return False
    Nothing -> throwError $ InternalError "Pan not found"

checkDL :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkDL merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  mdriverLicense <- DLQuery.findByDriverId personId
  case mdriverLicense of
    Just dlData -> do
      if verifyingDocumentType `elem` [ODC.AadhaarCard, ODC.PanCard]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue dlData.driverName mbDateOfBirthValue dlData.driverDob personId
        else return False
    Nothing -> throwError $ InternalError "DL not found"

checkAadhaar :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkAadhaar merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  aadhaarInfo <- QAadhaarCard.findByPrimaryKey personId
  case aadhaarInfo of
    Just aadhaarData -> do
      if verifyingDocumentType `elem` [ODC.PanCard, ODC.DriverLicense]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue aadhaarData.nameOnCard mbDateOfBirthValue (parseDateTime =<< aadhaarData.dateOfBirth) personId
        else return False
    Nothing -> throwError $ InternalError "Aadhaar not found"

checkGST :: Id Person.Person -> Maybe Text -> Flow ()
checkGST personId mbPanNumber = do
  mGstData <- DGQuery.findByDriverId personId
  case mGstData of
    Just gstData -> checkTwoPanNumber mbPanNumber gstData.panNumber
    Nothing -> throwError $ InternalError "GST not found"

validateDocument :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> ODC.DocumentType -> DriverDocument -> Flow ()
validateDocument merchantId merchantOpCityId personId mbNameValue mbDateOfBirthValue mbPanNumber verifyingDocumentType DriverDocument {..} = do
  let mbUtcDateOfBirthValue = parseDateTime =<< mbDateOfBirthValue
  case verifyingDocumentType of
    ODC.AadhaarCard -> do
      panChecked <-
        maybe
          (pure False)
          (\_ -> checkPan merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.AadhaarCard)
          panNumber
      unless panChecked $
        whenJust dlNumber $ \_ ->
          void $ checkDL merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.AadhaarCard
    ODC.PanCard -> do
      aadhaarChecked <-
        maybe
          (pure False)
          (\_ -> checkAadhaar merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.PanCard)
          aadhaarNumber
      unless aadhaarChecked $ do
        dlChecked <-
          maybe
            (pure False)
            (\_ -> checkDL merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.PanCard)
            dlNumber
        unless dlChecked $
          when (isJust gstNumber) $
            checkGST personId mbPanNumber
    ODC.DriverLicense -> do
      aadhaarChecked <-
        maybe
          (pure False)
          (\_ -> checkAadhaar merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.DriverLicense)
          aadhaarNumber
      unless aadhaarChecked $
        whenJust panNumber $ \_ ->
          void $ checkPan merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.DriverLicense
    ODC.GSTCertificate -> checkTwoPanNumber mbPanNumber panNumber
    _ -> return ()

checkTwoPanNumber :: Maybe Text -> Maybe Text -> Flow ()
checkTwoPanNumber mbExtractedValue mbVerifiedValue = do
  case (mbExtractedValue, mbVerifiedValue) of
    (Just extractedValue, Just verifiedValue) -> do
      unless (extractedValue == verifiedValue) $ throwError (MismatchDataError "GST not linked with existing PAN")
      return ()
    _ -> return ()

validateNameAndDOB :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Id Person.Person -> Flow Bool
validateNameAndDOB merchantId merchantOpCityId mbExtractedName mbVerifiedName mbExtractedDOB mbVerifiedDOB personId = do
  isNameValid <- compareNames merchantId merchantOpCityId mbExtractedName mbVerifiedName personId
  isDateOfBirthValid <- compareDateOfBirth mbExtractedDOB mbVerifiedDOB
  return (isNameValid && isDateOfBirthValid)

-- | Returns True if name compare is required for the given transporterConfig and verifyBy
isNameCompareRequired :: DTC.TransporterConfig -> DPan.VerifiedBy -> Bool
isNameCompareRequired transporterConfig verifyBy =
  isJust transporterConfig.validNameComparePercentage && verifyBy /= DPan.DASHBOARD_ADMIN && verifyBy /= DPan.DASHBOARD_USER
