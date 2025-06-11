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
  ( DriverRCReq (..),
    DriverRCRes,
    DriverPanReq (..),
    DriverPanRes,
    DriverGstinReq (..),
    DriverGstinRes,
    RCStatusReq (..),
    RCValidationReq (..),
    DriverAadhaarReq (..),
    DriverAadhaarRes,
    verifyRC,
    verifyPan,
    verifyGstin,
    verifyAadhaar,
    onVerifyRC,
    convertUTCTimetoDate,
    deactivateCurrentRC,
    linkRCStatus,
    deleteRC,
    getAllLinkedRCs,
    LinkedRC (..),
    DeleteRCReq (..),
    convertTextToUTC,
    makeFleetOwnerKey,
    mkIdfyVerificationEntity,
    mkHyperVergeVerificationEntity,
    validateRCResponse,
    VerificationReqRecord (..),
  )
where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as DO
import AWS.S3 as S3
import Control.Applicative ((<|>))
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import qualified Control.Monad.Extra as CME
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import Data.Text as T hiding (elem, find, length, map, null, zip)
import Data.Time (Day)
import Data.Time.Format
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverGstin as DGst
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.HyperVergeVerification as Domain
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.RCValidationRules
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow, VerificationFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (find)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import SharedLogic.DriverOnboarding
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as SCO
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as DGQuery
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import Storage.Queries.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetOwnerInformation as FOI
import qualified Storage.Queries.FleetOwnerInformation as QFOI
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
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime, -- updatable
    vehicleCategory :: Maybe DVC.VehicleCategory,
    airConditioned :: Maybe Bool,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    multipleRC :: Maybe Bool,
    vehicleDetails :: Maybe DriverVehicleDetails -- updatable
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverRCRes = APISuccess

data DriverPanReq = DriverPanReq
  { panNumber :: Text,
    imageId :: Text, --Image,
    driverId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverPanRes = APISuccess

data DriverGstinReq = DriverGstinReq
  { gstin :: Text,
    imageId :: Text, --Image,
    driverId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverGstinRes = APISuccess

data DriverAadhaarReq = DriverAadhaarReq
  { aadhaarNumber :: Text,
    aadhaarFrontImageId :: Text,
    aadhaarBackImageId :: Maybe Text,
    consent :: Bool,
    driverId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverAadhaarRes = APISuccess

data LinkedRC = LinkedRC
  { rcDetails :: VehicleRegistrationCertificateAPIEntity,
    rcActive :: Bool
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

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

prefixMatchedResult :: Text -> [Text] -> Bool
prefixMatchedResult rcNumber = DL.any (`T.isPrefixOf` rcNumber)

verifyRC ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverRCReq ->
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, _, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  documentVerificationConfig <- SCO.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId ODC.VehicleRegistrationCertificate (fromMaybe DVC.CAR req.vehicleCategory) Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.VehicleRegistrationCertificate))
  let checkPrefixOfRCNumber = null documentVerificationConfig.rcNumberPrefixList || prefixMatchedResult req.vehicleRegistrationCertNumber documentVerificationConfig.rcNumberPrefixList
  unless checkPrefixOfRCNumber $ throwError (InvalidRequest "RC number prefix is not valid")
  runRequestValidation validateDriverRCReq req
  blocked <- case person.role of
    Person.FLEET_OWNER -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
  when blocked $ throwError AccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (person.role == Person.DRIVER) $ do
    allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
    unless (length allLinkedRCs < (transporterConfig.rcLimit + (if isDashboard then 1 else 0))) $ throwError (RCLimitReached transporterConfig.rcLimit)
  let mbAirConditioned = maybe req.airConditioned (\category -> if category `elem` [DVC.CAR, DVC.AMBULANCE, DVC.BUS] then req.airConditioned else Just False) req.vehicleCategory
      (mbOxygen, mbVentilator) = maybe (req.oxygen, req.ventilator) (\category -> if category == DVC.AMBULANCE then (req.oxygen, req.ventilator) else (Just False, Just False)) req.vehicleCategory
  when
    ( isNothing req.vehicleDetails && isNothing req.dateOfRegistration && documentVerificationConfig.checkExtraction
        && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
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

  mVehicleRC <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationCertNumber
  encryptedRC <- encrypt req.vehicleRegistrationCertNumber
  let imageExtractionValidation = bool Domain.Skipped Domain.Success (isNothing req.dateOfRegistration && documentVerificationConfig.checkExtraction)
  Redis.whenWithLockRedis (rcVerificationLockKey req.vehicleRegistrationCertNumber) 60 $ do
    whenJust mVehicleRC $ \vehicleRC -> do
      when (isNothing req.multipleRC) $ checkIfVehicleAlreadyExists person.id vehicleRC -- backward compatibility
    case req.vehicleDetails of
      Just vDetails@DriverVehicleDetails {..} -> do
        vehicleDetails <-
          CQVD.findByMakeAndModelAndYear vehicleManufacturer vehicleModel vehicleModelYear
            |<|>| CQVD.findByMakeAndModelAndYear vehicleManufacturer vehicleModel Nothing
        void $ onVerifyRCHandler person (buildRCVerificationResponse vehicleDetails vehicleColour vehicleManufacturer vehicleModel) req.vehicleCategory mbAirConditioned req.imageId ((vehicleDetails <&> (.vehicleVariant)) <|> Just DV.HATCHBACK) vehicleDoors vehicleSeatBelts req.dateOfRegistration vDetails.vehicleModelYear mbOxygen mbVentilator Nothing (Just imageExtractionValidation) (Just encryptedRC) req.multipleRC req.imageId Nothing Nothing
      Nothing -> verifyRCFlow person merchantOpCityId req.vehicleRegistrationCertNumber req.imageId req.dateOfRegistration req.multipleRC req.vehicleCategory mbAirConditioned mbOxygen mbVentilator encryptedRC imageExtractionValidation
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById imageId_ >>= fromMaybeM (ImageNotFound imageId_.getId)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId_.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_.getId)
      unless (imageMetadata.imageType == ODC.VehicleRegistrationCertificate) $
        throwError (ImageInvalidType (show ODC.VehicleRegistrationCertificate) (show imageMetadata.imageType))
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path

    buildRCVerificationResponse vehicleDetails vehicleColour vehicleManufacturer vehicleModel =
      Verification.RCVerificationResponse
        { registrationDate = show <$> req.dateOfRegistration,
          registrationNumber = Just req.vehicleRegistrationCertNumber,
          fitnessUpto = Nothing,
          insuranceValidity = Nothing,
          vehicleClass = Nothing,
          vehicleCategory = Nothing,
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

verifyPan ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverPanReq ->
  Flow DriverPanRes
verifyPan isDashboard mbMerchant (personId, _, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  blocked <- case person.role of
    Person.FLEET_OWNER -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
  when blocked $ throwError AccountBlocked
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbPanVerificationService = merchantServiceUsageConfig.panVerificationService
  mdriverPanInformation <- DPQuery.findByDriverId person.id
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  case mbPanVerificationService of
    Just VI.HyperVerge -> do
      let panReq = DO.DriverPanReq {panNumber = req.panNumber, imageId1 = (Id req.imageId), imageId2 = Nothing, consent = True, nameOnCard = Nothing, dateOfBirth = Nothing, consentTimestamp = Nothing, validationStatus = Nothing, verifiedBy = Nothing, transactionId = Nothing, nameOnGovtDB = Nothing, docType = Nothing}
      void $ checkIfGenuineReq panReq person
      panCardDetails <- buildPanCard person Nothing Nothing Nothing
      DPQuery.create $ panCardDetails
    Just VI.Idfy -> do
      void $ callIdfy person mdriverPanInformation
    _ -> do
      panCardDetails <- buildPanCard person Nothing Nothing Nothing
      DPQuery.create $ panCardDetails
  case person.role of
    Person.FLEET_OWNER -> do
      panNumber <- encrypt req.panNumber
      QFOI.updatePanImage (Just panNumber) (Just req.imageId) person.id
    _ -> pure ()
  return Success
  where
    getImage :: Text -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById (Id imageId_) >>= fromMaybeM (ImageNotFound imageId_)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId_)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_)
      unless (imageMetadata.imageType == ODC.PanCard) $
        throwError (ImageInvalidType (show ODC.PanCard) (show imageMetadata.imageType))
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path
    callIdfy :: Person.Person -> Maybe DPan.DriverPanCard -> Flow APISuccess
    callIdfy person mdriverPanInformation = do
      image1 <- getImage req.imageId
      let extractReq =
            Verification.ExtractImageReq
              { image1 = image1,
                image2 = Nothing,
                driverId = person.id.getId
              }

      let validateExtractedPan resp = case resp.extractedPan of
            Just extractedPan -> do
              let extractedPanNo = removeSpaceAndDash <$> extractedPan.id_number
              unless (extractedPanNo == Just req.panNumber) $
                throwImageError (Id req.imageId) $
                  ImageDocumentNumberMismatch
                    (maybe "null" maskText extractedPanNo)
                    (maskText req.panNumber)
              pure extractedPan
            Nothing -> throwImageError (Id req.imageId) ImageExtractionFailed

      case mdriverPanInformation of
        Just driverPanInformation -> do
          let verificationStatus = driverPanInformation.verificationStatus
          when (verificationStatus == Documents.VALID) $
            throwError PanAlreadyLinked

          resp <- Verification.extractPanImage person.merchantId merchantOpCityId extractReq
          _ <- validateExtractedPan resp
          DPQuery.updateVerificationStatus Documents.VALID person.id
        Nothing -> do
          resp <- Verification.extractPanImage person.merchantId merchantOpCityId extractReq
          extractedPan <- validateExtractedPan resp
          panCardDetails <- buildPanCard person extractedPan.pan_type extractedPan.name_on_card extractedPan.date_of_birth
          DPQuery.create $ panCardDetails

      pure Success

    buildPanCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DPan.DriverPanCard
    buildPanCard person panType panName panDob = do
      panNoEnc <- encrypt req.panNumber
      now <- getCurrentTime
      uuid <- generateGUID
      let parsedDob = panDob >>= parseDateTime
      return $
        DPan.DriverPanCard
          { panCardNumber = panNoEnc,
            documentImageId1 = Id req.imageId,
            driverId = person.id,
            id = uuid,
            verificationStatus = Documents.VALID,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now,
            consent = True,
            docType = castTextToDomainType panType,
            consentTimestamp = now,
            documentImageId2 = Nothing,
            driverDob = parsedDob,
            driverName = Just person.firstName,
            driverNameOnGovtDB = panName,
            failedRules = [],
            verifiedBy = Just (if isDashboard then DPan.DASHBOARD else DPan.FRONTEND_SDK)
          }

    parseDateTime :: Text -> Maybe UTCTime
    parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

    checkIfGenuineReq :: (ServiceFlow m r) => API.Types.UI.DriverOnboardingV2.DriverPanReq -> Person.Person -> m ()
    checkIfGenuineReq API.Types.UI.DriverOnboardingV2.DriverPanReq {..} person = do
      (txnId, valStatus) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Cannot find necessary data for SDK response!!!!")) (return $ (,) <$> transactionId <*> validationStatus)
      hvResp <- Verification.verifySdkResp person.merchantId merchantOpCityId (VI.VerifySdkDataReq txnId)
      (respTxnId, respStatus, respUserDetails) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Invalid data recieved while validating data.")) (return $ (,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= txnId) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      when (Image.convertHVStatusToValidationStatus respStatus /= valStatus) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      case respUserDetails of
        VI.HVPanFlow (VI.PanFlow {pan = panFromResp, name = nameFromResp, dob = dobFromResp}) -> do
          panNum <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "PAN number not found in SDK validation response even though it's compulsory for Pan")) (return panFromResp)
          when (panNumber /= panNum) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (nameOnCard /= nameFromResp) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (isJust dateOfBirth && (formatUTCToDateString <$> dateOfBirth) /= (T.unpack <$> dobFromResp)) $ do
            logDebug $ "date of Birth and dob is : " <> show (formatUTCToDateString <$> dateOfBirth) <> " " <> show dobFromResp
            void $ Image.throwValidationError (Just imageId1) Nothing Nothing
        _ -> void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      where
        formatUTCToDateString :: UTCTime -> String
        formatUTCToDateString utcTime = formatTime defaultTimeLocale "%d-%m-%Y" utcTime

    castTextToDomainType :: Maybe Text -> Maybe DPan.PanType
    castTextToDomainType panType = case panType of
      Just "Individual" -> Just DPan.INDIVIDUAL
      Just _ -> Just DPan.BUSINESS
      Nothing -> Nothing

verifyGstin ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverGstinReq ->
  Flow DriverPanRes
verifyGstin isDashboard mbMerchant (personId, _, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  blocked <- case person.role of
    Person.FLEET_OWNER -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
  when blocked $ throwError AccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbGstVerificationService = merchantServiceUsageConfig.gstVerificationService
  mdriverGstInformation <- DGQuery.findByDriverId person.id

  case mbGstVerificationService of
    Just VI.Idfy -> do
      void $ callIdfy person mdriverGstInformation
    _ -> do
      gstCardDetails <- buildGstinCard person Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      DGQuery.create $ gstCardDetails

  case person.role of
    Person.FLEET_OWNER -> do
      gstin <- encrypt req.gstin
      QFOI.updateGstImage (Just gstin) (Just req.imageId) person.id
    _ -> pure ()
  return Success
  where
    getImage :: Text -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById (Id imageId_) >>= fromMaybeM (ImageNotFound imageId_)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId_)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_)
      unless (imageMetadata.imageType == ODC.GSTCertificate) $
        throwError (ImageInvalidType (show ODC.GSTCertificate) (show imageMetadata.imageType))
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path
    buildGstinCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DGst.DriverGstin
    buildGstinCard person address constitution_of_business date_of_liability is_provisional legal_name trade_name type_of_registration valid_from valid_upto = do
      gstinEnc <- encrypt req.gstin
      now <- getCurrentTime
      uuid <- generateGUID
      return $
        DGst.DriverGstin
          { documentImageId1 = Id req.imageId,
            driverId = person.id,
            id = uuid,
            verificationStatus = Documents.VALID,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            address = address,
            constitutionOfBusiness = constitution_of_business,
            updatedAt = now,
            documentImageId2 = Nothing,
            dateOfLiability = date_of_liability >>= parseDateTime,
            driverName = Just person.firstName,
            gstin = gstinEnc,
            isProvisional = is_provisional,
            legalName = legal_name,
            tradeName = trade_name,
            typeOfRegistration = type_of_registration,
            validFrom = valid_from >>= parseDateTime,
            validUpto = valid_upto >>= parseDateTime,
            verifiedBy = pure $ if isDashboard then DPan.DASHBOARD else DPan.FRONTEND_SDK
          }
    parseDateTime :: Text -> Maybe UTCTime
    parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack
    callIdfy :: Person.Person -> Maybe DGst.DriverGstin -> Flow APISuccess
    callIdfy person mdriverGstInformation = do
      image1 <- getImage req.imageId
      let extractReq =
            Verification.ExtractImageReq
              { image1 = image1,
                image2 = Nothing,
                driverId = person.id.getId
              }

      let validateExtractedGst resp = case resp.extractedGST of
            Just extractedGST -> do
              let extractedGstNo = removeSpaceAndDash <$> extractedGST.gstin
              unless (extractedGstNo == Just req.gstin) $
                throwImageError (Id req.imageId) $
                  ImageDocumentNumberMismatch
                    (maybe "null" maskText extractedGstNo)
                    (maskText req.gstin)
              pure extractedGST
            Nothing -> throwImageError (Id req.imageId) ImageExtractionFailed

      case mdriverGstInformation of
        Just driverGstInformation -> do
          let verificationStatus = driverGstInformation.verificationStatus
          when (verificationStatus == Documents.VALID) $
            throwError GstAlreadyLinked

          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          _ <- validateExtractedGst resp
          DGQuery.updateVerificationStatus Documents.VALID person.id
        Nothing -> do
          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          extractedGst <- validateExtractedGst resp
          gstCardDetails <- buildGstinCard person extractedGst.address extractedGst.constitution_of_business extractedGst.date_of_liability extractedGst.is_provisional extractedGst.legal_name extractedGst.trade_name extractedGst.type_of_registration extractedGst.valid_from extractedGst.valid_upto
          DGQuery.create $ gstCardDetails
      pure Success

verifyAadhaar ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverAadhaarReq ->
  Flow DriverAadhaarRes
verifyAadhaar _isDashboard mbMerchant (personId, merchantId, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  blocked <- case person.role of
    Person.FLEET_OWNER -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound personId.getId)
      return res.blocked
  when blocked $ throwError AccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  aadhaarInfo <- QAadhaarCard.findByPrimaryKey person.id
  whenJust aadhaarInfo $ \aadhaarInfoData -> do
    when (aadhaarInfoData.verificationStatus == Documents.VALID) $ throwError AadhaarAlreadyLinked
  image1 <- getImage req.aadhaarFrontImageId
  image2 <- case req.aadhaarBackImageId of
    Just backImageId -> do
      image <- getImage backImageId
      return $ Just image
    Nothing -> return Nothing
  let extractReq =
        Verification.ExtractAadhaarImageReq
          { image1 = image1,
            image2 = image2,
            driverId = person.id.getId,
            consent = if req.consent then "yes" else "no"
          }
  resp <- Verification.extractAadhaarImage person.merchantId merchantOpCityId extractReq
  case resp.extractedAadhaar of
    Just extractedAadhaarData -> do
      let extractedAadhaarOutputData = extractedAadhaarData.extraction_output
      let extractedAadhaarNumber = removeSpaceAndDash <$> extractedAadhaarOutputData.id_number
      unless (extractedAadhaarNumber == Just req.aadhaarNumber) $
        throwImageError (Id req.aadhaarFrontImageId) $
          ImageDocumentNumberMismatch
            (maybe "null" maskText extractedAadhaarNumber)
            (maskText req.aadhaarNumber)
      aadhaarCard <- makeAadhaarCardEntity person.id extractedAadhaarOutputData req
      QAadhaarCard.upsertAadhaarRecord aadhaarCard
      pure extractedAadhaarData
    Nothing -> throwImageError (Id req.aadhaarFrontImageId) ImageExtractionFailed
  case person.role of
    Person.FLEET_OWNER -> do
      aadhaarNumber <- encrypt req.aadhaarNumber
      QFOI.updateAadhaarImage (Just aadhaarNumber) (Just req.aadhaarFrontImageId) req.aadhaarBackImageId person.id
    _ -> pure ()
  return Success
  where
    makeAadhaarCardEntity driverId extractedAadhaar aadhaarReq = do
      currTime <- getCurrentTime
      aadhaarHash <- getDbHash aadhaarReq.aadhaarNumber
      return $
        DAadhaarCard.AadhaarCard
          { driverId = driverId,
            createdAt = currTime,
            updatedAt = currTime,
            aadhaarNumberHash = Just aadhaarHash,
            dateOfBirth = extractedAadhaar.date_of_birth,
            driverGender = extractedAadhaar.gender,
            aadhaarBackImageId = Id <$> aadhaarReq.aadhaarBackImageId,
            aadhaarFrontImageId = Just (Id aadhaarReq.aadhaarFrontImageId),
            address = extractedAadhaar.address,
            verificationStatus = Documents.VALID,
            consent = aadhaarReq.consent,
            consentTimestamp = currTime,
            driverImage = Nothing,
            driverImagePath = Nothing,
            maskedAadhaarNumber = Just $ maskText aadhaarReq.aadhaarNumber,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            nameOnCard = extractedAadhaar.name_on_card
          }

    getImage :: Text -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById (Id imageId_) >>= fromMaybeM (ImageNotFound imageId_)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId_)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_)
      unless (imageMetadata.imageType == ODC.AadhaarCard) $
        throwError (ImageInvalidType (show ODC.AadhaarCard) (show imageMetadata.imageType))
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path

verifyRCFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe Bool -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> EncryptedHashedField 'AsEncrypted Text -> Domain.ImageExtractionValidation -> Flow ()
verifyRCFlow person merchantOpCityId rcNumber imageId dateOfRegistration multipleRC mbVehicleCategory mbAirConditioned mbOxygen mbVentilator encryptedRC imageExtractionValidation = do
  now <- getCurrentTime
  verifyRes <-
    Verification.verifyRC person.merchantId
      merchantOpCityId
      Nothing
      Verification.VerifyRCReq {rcNumber = rcNumber, driverId = person.id.getId}
  case verifyRes.verifyRCResp of
    Verification.AsyncResp res -> do
      case res.requestor of
        VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person res.requestId now imageExtractionValidation multipleRC encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId Nothing Nothing
        VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person res.requestId now imageExtractionValidation multipleRC encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId Nothing Nothing res.transactionId
        _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> (show res.requestor))
      CQO.setVerificationPriorityList person.id verifyRes.remPriorityList
    Verification.SyncResp res -> do
      void $ onVerifyRC person Nothing res (Just verifyRes.remPriorityList) (Just imageExtractionValidation) (Just encryptedRC) multipleRC imageId Nothing Nothing Nothing

mkIdfyVerificationEntity :: MonadFlow m => Person.Person -> Text -> UTCTime -> Domain.ImageExtractionValidation -> Maybe Bool -> EncryptedHashedField 'AsEncrypted Text -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> m Domain.IdfyVerification
mkIdfyVerificationEntity person requestId now imageExtractionValidation multipleRC encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbStatus = do
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
        multipleRC,
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

mkHyperVergeVerificationEntity :: MonadFlow m => Person.Person -> Text -> UTCTime -> Domain.ImageExtractionValidation -> Maybe Bool -> EncryptedHashedField 'AsEncrypted Text -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> Maybe Text -> m Domain.HyperVergeVerification
mkHyperVergeVerificationEntity person requestId now imageExtractionValidation multipleRC encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbStatus transactionId = do
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
        multipleRC,
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

onVerifyRC :: VerificationFlow m r => Person.Person -> Maybe VerificationReqRecord -> VT.RCVerificationResponse -> Maybe [VT.VerificationService] -> Maybe Domain.ImageExtractionValidation -> Maybe (EncryptedHashedField 'AsEncrypted Text) -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> Maybe VT.VerificationService -> m AckResponse
onVerifyRC person mbVerificationReq rcVerificationResponse mbRemPriorityList mbImageExtractionValidation mbEncryptedRC multipleRC imageId mbRetryCnt mbReqStatus mbServiceName = do
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
      void $ onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned (maybe "" (.documentImageId1) mbVerificationReq) Nothing Nothing Nothing Nothing Nothing mbOxygen mbVentilator mbRemPriorityList mbImageExtractionValidation' mbEncryptedRC' multipleRC imageId mbRetryCnt mbReqStatus
      return Ack

onVerifyRCHandler :: VerificationFlow m r => Person.Person -> VT.RCVerificationResponse -> Maybe DVC.VehicleCategory -> Maybe Bool -> Id Image.Image -> Maybe DV.VehicleVariant -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe [VT.VerificationService] -> Maybe Domain.ImageExtractionValidation -> Maybe (EncryptedHashedField 'AsEncrypted Text) -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> m ()
onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned mbDocumentImageId mbVehicleVariant mbVehicleDoors mbVehicleSeatBelts mbDateOfRegistration mbVehicleModelYear mbOxygen mbVentilator mbRemPriorityList mbImageExtractionValidation mbEncryptedRC multipleRC imageId mbRetryCnt mbReqStatus' = do
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
      rcInput = createRCInput mbVehicleCategory mbFleetOwnerId mbDocumentImageId mbDateOfRegistration mbVehicleModelYear mbGrossVehicleWeight mbUnladdenWeight
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
                then Just ("Document expired: " <> field)
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
          resVerifyRes <- try @_ @SomeException $ Verification.verifyRC person.merchantId person.merchantOperatingCityId (Just remPriorityList) (Verification.VerifyRCReq {rcNumber = rcNum, driverId = person.id.getId})
          case resVerifyRes of
            Left _ -> initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures
            Right verifyRes -> do
              case verifyRes.verifyRCResp of
                Verification.AsyncResp res -> do
                  case res.requestor of
                    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person res.requestId now imageExtractionValidation multipleRC encryptedRC mbDateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbReqStatus
                    VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperVergeVerificationEntity person res.requestId now imageExtractionValidation multipleRC encryptedRC mbDateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbReqStatus res.transactionId
                    _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> T.pack (show res.requestor))
                  CQO.setVerificationPriorityList person.id verifyRes.remPriorityList
                Verification.SyncResp resp -> do
                  onVerifyRCHandler person resp mbVehicleCategory mbAirConditioned mbDocumentImageId mbVehicleVariant mbVehicleDoors mbVehicleSeatBelts mbDateOfRegistration mbVehicleModelYear mbOxygen mbVentilator (Just verifyRes.remPriorityList) mbImageExtractionValidation mbEncryptedRC multipleRC imageId mbRetryCnt mbReqStatus
    else initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures
  where
    createRCInput :: Maybe DVC.VehicleCategory -> Maybe Text -> Id Image.Image -> Maybe UTCTime -> Maybe Int -> Maybe Float -> Maybe Float -> CreateRCInput
    createRCInput vehicleCategory fleetOwnerId documentImageId dateOfRegistration vehicleModelYear mbGrossVehicleWeight mbUnladdenWeight =
      CreateRCInput
        { registrationNumber = rcVerificationResponse.registrationNumber,
          fitnessUpto = convertTextToUTC rcVerificationResponse.fitnessUpto,
          fleetOwnerId,
          vehicleCategory,
          airConditioned = mbAirConditioned,
          oxygen = mbOxygen,
          ventilator = mbVentilator,
          documentImageId,
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
            _ -> vehicleVariant
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
            updatedAt = now
          }
    initiateRCCreation transporterConfig mVehicleRC now mbFleetOwnerId allFailures = do
      case mVehicleRC of
        Just vehicleRC -> do
          -- upsert vehicleRC
          RCQuery.upsert vehicleRC
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" rcVerificationResponse.registrationNumber))
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
          [ if not fuelValid then Just ("Invalid fuel type : " <> show rc.fuelType) else Nothing,
            if not vehicleClassValid then Just ("Invalid vehicle class : " <> show rc.vehicleClass) else Nothing,
            if not manufacturerValid then Just ("Invalid OEM : " <> show rc.manufacturer) else Nothing,
            if not vehicleAgeValid then Just ("Invalid manufacturing: " <> show rc.mYManufacturing) else Nothing
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
  unless (rc.verificationStatus == Documents.VALID) $ throwError (InvalidRequest "Can't perform activate/inactivate operations on invalid RC!")
  now <- getCurrentTime
  if req.isActivate
    then do
      validated <- validateRCActivation driverId merchantOpCityId rc
      when validated $ activateRC driverInfo merchantId merchantOpCityId now rc
    else do
      deactivateRC rc driverId
  return Success

deactivateRC :: Domain.VehicleRegistrationCertificate -> Id Person.Person -> Flow ()
deactivateRC rc driverId = do
  activeAssociation <- DAQuery.findActiveAssociationByRC rc.id True >>= fromMaybeM ActiveRCNotFound
  unless (activeAssociation.driverId == driverId) $ throwError (InvalidRequest "Driver can't deactivate RC which is not active with them")
  removeVehicle driverId
  DAQuery.deactivateRCForDriver False driverId rc.id
  return ()

removeVehicle :: Id Person.Person -> Flow ()
removeVehicle driverId = do
  isOnRide <- DIQuery.findByDriverIdActiveRide (cast driverId)
  when (isJust isOnRide) $ throwError RCVehicleOnRide
  VQuery.deleteById driverId -- delete the vehicle entry too for the driver

validateRCActivation :: Id Person.Person -> Id DMOC.MerchantOperatingCity -> Domain.VehicleRegistrationCertificate -> Flow Bool
validateRCActivation driverId merchantOpCityId rc = do
  now <- getCurrentTime
  _ <- DAQuery.findLinkedByRCIdAndDriverId driverId rc.id now >>= fromMaybeM (InvalidRequest "RC not linked to driver. Please link.")

  -- check if rc is already active to other driver
  mActiveAssociation <- DAQuery.findActiveAssociationByRC rc.id True
  case mActiveAssociation of
    Just activeAssociation -> do
      if activeAssociation.driverId == driverId
        then return False
        else do
          deactivateIfWeCanDeactivate activeAssociation.driverId now (deactivateRC rc)
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
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast oldDriverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      mLastRideAssigned <- RQuery.findLastRideAssigned oldDriverId
      case mLastRideAssigned of
        Just lastRide -> do
          if nominalDiffTimeToSeconds (diffUTCTime now lastRide.createdAt) > transporterConfig.automaticRCActivationCutOff
            then deactivateFunc oldDriverId
            else throwError RCActiveOnOtherAccount
        Nothing -> do
          -- if driver didn't take any ride yet
          person <- Person.findById oldDriverId >>= fromMaybeM (PersonNotFound oldDriverId.getId)
          if nominalDiffTimeToSeconds (diffUTCTime now person.createdAt) > transporterConfig.automaticRCActivationCutOff
            then deactivateFunc oldDriverId
            else throwError RCActiveOnOtherAccount

checkIfVehicleAlreadyExists :: Id Person.Person -> Domain.VehicleRegistrationCertificate -> Flow ()
checkIfVehicleAlreadyExists driverId rc = do
  rcNumber <- decrypt rc.certificateNumber
  mVehicle <- VQuery.findByRegistrationNo rcNumber
  case mVehicle of
    Just vehicle -> unless (vehicle.driverId == driverId) $ throwError RCActiveOnOtherAccount
    Nothing -> return ()

activateRC :: DI.DriverInformation -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Domain.VehicleRegistrationCertificate -> Flow ()
activateRC driverInfo merchantId merchantOpCityId now rc = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverInfo.driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (transporterConfig.requiresOnboardingInspection == Just True) $ do
    unless driverInfo.enabled $ throwError (InvalidRequest "Driver is not enabled")
    unless (fromMaybe False rc.approved) $ throwError (InvalidRequest "Vehicle is not approved")
  deactivateCurrentRC driverInfo.driverId
  addVehicleToDriver transporterConfig
  DAQuery.activateRCForDriver driverInfo.driverId rc.id now
  return ()
  where
    addVehicleToDriver transporterConfig = do
      rcNumber <- decrypt rc.certificateNumber
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == DV.SUV) $
          DIQuery.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi driverInfo.driverId
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
      person <- Person.findById driverInfo.driverId >>= fromMaybeM (PersonNotFound driverInfo.driverId.getId)
      -- driverStats <- runInReplica $ QDriverStats.findById driverInfo.driverId >>= fromMaybeM DriverInfoNotFound
      let vehicle = makeFullVehicleFromRC cityVehicleServiceTiers driverInfo person merchantId rcNumber rc merchantOpCityId now Nothing
      VQuery.create vehicle

deactivateCurrentRC :: Id Person.Person -> Flow ()
deactivateCurrentRC driverId = do
  mActiveAssociation <- DAQuery.findActiveAssociationByDriver driverId True
  case mActiveAssociation of
    Just association -> do
      rc <- RCQuery.findById association.rcId >>= fromMaybeM (RCNotFound "")
      deactivateRC rc driverId -- call deativate RC flow
    Nothing -> do
      removeVehicle driverId
      return ()

deleteRC :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DeleteRCReq -> Bool -> Flow APISuccess
deleteRC (driverId, _, _) DeleteRCReq {..} isOldFlow = do
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  mAssoc <- DAQuery.findActiveAssociationByRC rc.id True
  case (mAssoc, isOldFlow) of
    (Just assoc, False) -> do
      when (assoc.driverId == driverId) $ throwError (InvalidRequest "Deactivate RC first to delete!")
    (Just _, True) -> deactivateRC rc driverId
    (_, _) -> return ()
  DAQuery.endAssociationForRC driverId rc.id
  return Success

getAllLinkedRCs :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow [LinkedRC]
getAllLinkedRCs (driverId, _, _) = do
  allLinkedRCs <- DAQuery.findAllLinkedByDriverId driverId
  rcs <- RCQuery.findAllById (map (.rcId) allLinkedRCs)
  let activeRcs = buildRcHM allLinkedRCs
  mapM (getCombinedRcData activeRcs) rcs
  where
    getCombinedRcData activeRcs rc = do
      rcNo <- decrypt rc.certificateNumber
      return $
        LinkedRC
          { rcActive = fromMaybe False $ HM.lookup rc.id.getId activeRcs <&> (.isRcActive),
            rcDetails = makeRCAPIEntity rc rcNo
          }

rcVerificationLockKey :: Text -> Text
rcVerificationLockKey rcNumber = "VehicleRC::RCNumber-" <> rcNumber

makeFleetOwnerKey :: Text -> Text
makeFleetOwnerKey vehicleNo = "FleetOwnerId:PersonId-" <> vehicleNo
