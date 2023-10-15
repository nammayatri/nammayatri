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
    RCStatusReq (..),
    verifyRC,
    onVerifyRC,
    convertUTCTimetoDate,
    deactivateCurrentRC,
    linkRCStatus,
    deleteRC,
    getAllLinkedRCs,
    LinkedRC (..),
    DeleteRCReq (..),
    activateRC,
    validateRCActivation,
    convertTextToUTC,
    makeFleetOwnerKey,
  )
where

import AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.HashMap as HML
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Text as T hiding (find, length, map, null, zip)
import qualified Data.Time as DT
import qualified Data.Time.Calendar.OrdinalDate as TO
import qualified Domain.Types.DriverOnboarding.DriverRCAssociation as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as ODC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Vehicle as Vehicle
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude hiding (find)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Merchant.OnboardingDocumentConfig as SCO
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.DriverInformation as DIQuery
import Storage.Queries.DriverOnboarding.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.OperatingCity as QCity
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.Person as Person
import Storage.Queries.Ride as RQuery
import qualified Storage.Queries.Vehicle as VQuery
import Tools.Error
import qualified Tools.Verification as Verification

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    multipleRC :: Maybe Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

data LinkedRC = LinkedRC
  { rcDetails :: Domain.VehicleRegistrationCertificateAPIEntity,
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

validateDriverRCReq :: Text -> Validate DriverRCReq
validateDriverRCReq rcNumberPrefix DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` (string (T.unpack rcNumberPrefix) <> star (latinUC \/ digit \/ ","))

verifyRC ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant) ->
  DriverRCReq ->
  Maybe Vehicle.Variant -> -- in case hardcoded variant passed from dashboard, then ignore variant coming from IDFY
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, merchantId) req@DriverRCReq {..} mbVariant = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  onboardingDocumentConfig <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RC))
  runRequestValidation (validateDriverRCReq onboardingDocumentConfig.rcNumberPrefix) req
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  operatingCity' <- case mbMerchant of
    Just merchant -> QCity.findEnabledCityByMerchantIdAndName merchant.id $ T.toLower req.operatingCity
    Nothing -> QCity.findEnabledCityByName $ T.toLower req.operatingCity
  when (null operatingCity') $
    throwError $ InvalidOperatingCity req.operatingCity
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)

  allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
  unless (length allLinkedRCs < transporterConfig.rcLimit) $ throwError (RCLimitReached transporterConfig.rcLimit)

  when
    ( isNothing dateOfRegistration && onboardingDocumentConfig.checkExtraction
        && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
    )
    $ do
      image <- getImage imageId
      resp <-
        Verification.extractRCImage person.merchantId $
          Verification.ExtractImageReq {image1 = image, image2 = Nothing, driverId = person.id.getId}
      case resp.extractedRC of
        Just extractedRC -> do
          let extractRCNumber = removeSpaceAndDash <$> extractedRC.rcNumber
          let rcNumber = removeSpaceAndDash <$> Just vehicleRegistrationCertNumber
          -- disable this check for debugging with mock-idfy
          unless (extractRCNumber == rcNumber) $
            throwImageError imageId $ ImageDocumentNumberMismatch (maybe "null" maskText extractRCNumber) (maybe "null" maskText rcNumber)
        Nothing -> throwImageError imageId ImageExtractionFailed

  mVehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleRegistrationCertNumber
  Redis.whenWithLockRedis (rcVerificationLockKey vehicleRegistrationCertNumber) 60 $ do
    case mVehicleRC of
      Just vehicleRC -> do
        when (isJust mbVariant) $
          RCQuery.updateVehicleVariant vehicleRC.id mbVariant -- update vehicleVariant of RC is passed hardcoded from dashboard
        when (isNothing multipleRC) $ checkIfVehicleAlreadyExists person.id vehicleRC -- backward compatibility
        mRCAssociation <- DAQuery.findLatestByRCIdAndDriverId vehicleRC.id person.id
        case mRCAssociation of
          Just assoc -> do
            now <- getCurrentTime
            when (maybe True (now >) assoc.associatedTill) $ -- if that association is old, create new association for that driver
              createRCAssociation person.id vehicleRC
          Nothing -> do
            -- if no association to driver, create one. No need to verify RC as already verified except in fallback case
            if isNothing dateOfRegistration
              then createRCAssociation person.id vehicleRC
              else verifyRCFlow person onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant
      Nothing ->
        verifyRCFlow person onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant

  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById imageId_ >>= fromMaybeM (ImageNotFound imageId_.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId_.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_.getId)
      unless (imageMetadata.imageType == Image.VehicleRegistrationCertificate) $
        throwError (ImageInvalidType (show Image.VehicleRegistrationCertificate) (show imageMetadata.imageType))
      S3.get $ T.unpack imageMetadata.s3Path

    createRCAssociation driverId rc = do
      driverRCAssoc <- Domain.makeRCAssociation driverId rc.id (convertTextToUTC (Just "2099-12-12"))
      DAQuery.create driverRCAssoc
      when (isNothing multipleRC) $ do
        rcNumber <- decrypt rc.certificateNumber
        let rcStatusReq =
              RCStatusReq
                { rcNo = rcNumber,
                  isActivate = True
                }
        void $ linkRCStatus (driverId, merchantId) rcStatusReq

verifyRCFlow :: Person.Person -> Bool -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe Bool -> Maybe Vehicle.Variant -> Flow ()
verifyRCFlow person imageExtraction rcNumber imageId dateOfRegistration multipleRC mbVariant = do
  now <- getCurrentTime
  encryptedRC <- encrypt rcNumber
  let imageExtractionValidation =
        if isNothing dateOfRegistration && imageExtraction
          then Domain.Success
          else Domain.Skipped
  verifyRes <-
    Verification.verifyRCAsync person.merchantId $
      Verification.VerifyRCAsyncReq {rcNumber = rcNumber, driverId = person.id.getId}
  idfyVerificationEntity <- mkIdfyVerificationEntity verifyRes.requestId now imageExtractionValidation encryptedRC
  IVQuery.create idfyVerificationEntity
  where
    mkIdfyVerificationEntity requestId now imageExtractionValidation encryptedRC = do
      id <- generateGUID
      return $
        Domain.IdfyVerification
          { id,
            driverId = person.id,
            documentImageId1 = imageId,
            documentImageId2 = Nothing,
            requestId,
            docType = Image.VehicleRegistrationCertificate,
            documentNumber = encryptedRC,
            driverDateOfBirth = Nothing,
            imageExtractionValidation = imageExtractionValidation,
            issueDateOnDoc = dateOfRegistration,
            status = "pending",
            idfyResponse = Nothing,
            multipleRC,
            dashboardPassedVehicleVariant = mbVariant,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Domain.IdfyVerification -> Idfy.RCVerificationOutput -> Flow AckResponse
onVerifyRC verificationReq output = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  if verificationReq.imageExtractionValidation == Domain.Skipped
    && compareRegistrationDates output.registration_date verificationReq.issueDateOnDoc
    then IVQuery.updateExtractValidationStatus verificationReq.requestId Domain.Failed >> return Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      rCConfigs <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RC))
      rCInsuranceConfigs <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RCInsurance >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RCInsurance))
      mEncryptedRC <- encrypt `mapM` output.registration_number
      modelNamesHashMap <- asks (.modelNamesHashMap)
      let mbFitnessEpiry = convertTextToUTC output.fitness_upto <|> Just (DT.UTCTime (TO.fromOrdinalDate 1900 1) 0)
          mVehicleRC = createRC rCConfigs rCInsuranceConfigs output id verificationReq.documentImageId1 now verificationReq.dashboardPassedVehicleVariant modelNamesHashMap <$> mEncryptedRC <*> mbFitnessEpiry
      case mVehicleRC of
        Just vehicleRC -> do
          RCQuery.upsert vehicleRC

          -- linking to driver
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" output.registration_number))
          driverRCAssoc <- Domain.makeRCAssociation person.id rc.id (convertTextToUTC (Just "2099-12-12"))
          DAQuery.create driverRCAssoc
          return Ack
        _ -> return Ack

compareRegistrationDates :: Maybe Text -> Maybe UTCTime -> Bool
compareRegistrationDates actualDate providedDate =
  isJust providedDate
    && ((convertUTCTimetoDate <$> providedDate) /= (convertUTCTimetoDate <$> convertTextToUTC actualDate))

linkRCStatus :: (Id Person.Person, Id DM.Merchant) -> RCStatusReq -> Flow APISuccess
linkRCStatus (driverId, merchantId) req@RCStatusReq {..} = do
  driverInfo <- DIQuery.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  unless (driverInfo.subscribed || transporterConfig.openMarketUnBlocked) $ throwError (RCActivationFailedPaymentDue driverId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  unless (rc.verificationStatus == Domain.VALID) $ throwError (InvalidRequest "Can't perform activate/inactivate operations on invalid RC!")
  now <- getCurrentTime
  if req.isActivate
    then do
      validateRCActivation driverId merchantId rc
      activateRC driverId merchantId now rc
    else do
      deactivateRC rc driverId
  return Success

deactivateRC :: Domain.VehicleRegistrationCertificate -> Id Person.Person -> Flow ()
deactivateRC rc driverId = do
  activeAssociation <- DAQuery.findActiveAssociationByRC rc.id >>= fromMaybeM ActiveRCNotFound
  unless (activeAssociation.driverId == driverId) $ throwError (InvalidRequest "Driver can't deactivate RC which is not active with them")
  removeVehicle driverId
  DAQuery.deactivateRCForDriver driverId rc.id
  return ()

removeVehicle :: Id Person.Person -> Flow ()
removeVehicle driverId = do
  isOnRide <- DIQuery.findByDriverIdActiveRide (cast driverId)
  when (isJust isOnRide) $ throwError RCVehicleOnRide
  VQuery.deleteById driverId -- delete the vehicle entry too for the driver

validateRCActivation :: Id Person.Person -> Id DM.Merchant -> Domain.VehicleRegistrationCertificate -> Flow ()
validateRCActivation driverId merchantId rc = do
  now <- getCurrentTime
  _ <- DAQuery.findLinkedByRCIdAndDriverId driverId rc.id now >>= fromMaybeM (InvalidRequest "RC not linked to driver. Please link.")

  -- check if rc is already active to other driver
  mActiveAssociation <- DAQuery.findActiveAssociationByRC rc.id
  case mActiveAssociation of
    Just activeAssociation -> do
      when (activeAssociation.driverId == driverId) $ throwError (InvalidRequest "RC already active with driver requested")
      deactivateIfWeCanDeactivate activeAssociation.driverId now (deactivateRC rc)
    Nothing -> do
      -- check if vehicle of that rc number is already with other driver
      mVehicle <- VQuery.findByRegistrationNo =<< decrypt rc.certificateNumber
      case mVehicle of
        Just vehicle -> do
          if vehicle.driverId /= driverId
            then deactivateIfWeCanDeactivate vehicle.driverId now removeVehicle
            else removeVehicle driverId
        Nothing -> return ()
  where
    deactivateIfWeCanDeactivate :: Id Person.Person -> UTCTime -> (Id Person.Person -> Flow ()) -> Flow ()
    deactivateIfWeCanDeactivate oldDriverId now deactivateFunc = do
      transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
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

activateRC :: Id Person.Person -> Id DM.Merchant -> UTCTime -> Domain.VehicleRegistrationCertificate -> Flow ()
activateRC driverId merchantId now rc = do
  deactivateCurrentRC driverId
  addVehicleToDriver
  DAQuery.activateRCForDriver driverId rc.id now
  return ()
  where
    addVehicleToDriver = do
      rcNumber <- decrypt rc.certificateNumber
      fleetOwnerId <- Redis.safeGet $ makeFleetOwnerKey rcNumber
      transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == Vehicle.SUV) $
          DIQuery.updateDriverDowngradeTaxiForSuv driverId transporterConfig.canSuvDowngradeToTaxi
      Redis.del $ makeFleetOwnerKey rcNumber
      let vehicle = Domain.makeVehicleFromRC now driverId merchantId rcNumber rc fleetOwnerId
      VQuery.create vehicle

deactivateCurrentRC :: Id Person.Person -> Flow ()
deactivateCurrentRC driverId = do
  mActiveAssociation <- DAQuery.findActiveAssociationByDriver driverId
  case mActiveAssociation of
    Just association -> do
      rc <- RCQuery.findById association.rcId >>= fromMaybeM (RCNotFound "")
      deactivateRC rc driverId -- call deativate RC flow
    Nothing -> do
      removeVehicle driverId
      return ()

deleteRC :: (Id Person.Person, Id DM.Merchant) -> DeleteRCReq -> Bool -> Flow APISuccess
deleteRC (driverId, _) DeleteRCReq {..} isOldFlow = do
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  mAssoc <- DAQuery.findActiveAssociationByRC rc.id
  case (mAssoc, isOldFlow) of
    (Just assoc, False) -> do
      when (assoc.driverId == driverId) $ throwError (InvalidRequest "Deactivate RC first to delete!")
    (Just _, True) -> deactivateRC rc driverId
    (_, _) -> return ()
  DAQuery.endAssociationForRC driverId rc.id
  return Success

getAllLinkedRCs :: (Id Person.Person, Id DM.Merchant) -> Flow [LinkedRC]
getAllLinkedRCs (driverId, _) = do
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
            rcDetails = Domain.makeRCAPIEntity rc rcNo
          }

createRC ::
  ODC.OnboardingDocumentConfig ->
  ODC.OnboardingDocumentConfig ->
  Idfy.RCVerificationOutput ->
  Id Domain.VehicleRegistrationCertificate ->
  Id Image.Image ->
  UTCTime ->
  Maybe Vehicle.Variant ->
  HML.Map Text Text ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.VehicleRegistrationCertificate
createRC rcconfigs rcInsurenceConfigs output id imageId now mbVariant modelNamesHashMap edl expiry = do
  let insuranceValidity = convertTextToUTC output.insurance_validity
  let vehicleClass = output.vehicle_class
  let vehicleCapacity = (readMaybe . T.unpack) =<< output.seating_capacity
  let manufacturer = (readMaybe . T.unpack) =<< output.manufacturer
  let (verificationStatus, variant) = validateRCStatus mbVariant rcconfigs rcInsurenceConfigs expiry insuranceValidity vehicleClass now vehicleCapacity manufacturer
  Domain.VehicleRegistrationCertificate
    { id,
      documentImageId = imageId,
      certificateNumber = edl,
      fitnessExpiry = expiry,
      permitExpiry = convertTextToUTC output.permit_validity_upto,
      pucExpiry = convertTextToUTC output.puc_validity_upto,
      vehicleClass,
      vehicleVariant = variant,
      vehicleManufacturer = output.manufacturer <|> output.manufacturer_model,
      vehicleCapacity,
      vehicleModel = updateModel =<< (output.manufacturer_model <|> output.m_y_manufacturing),
      vehicleColor = output.color <|> output.colour,
      vehicleEnergyType = output.fuel_type,
      insuranceValidity,
      verificationStatus,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }
  where
    updateModel modelFromIdfy = Just do
      case HML.lookup modelFromIdfy modelNamesHashMap of
        Just newModelMapped -> newModelMapped
        Nothing -> ""

validateRCStatus :: Maybe Vehicle.Variant -> ODC.OnboardingDocumentConfig -> ODC.OnboardingDocumentConfig -> UTCTime -> Maybe UTCTime -> Maybe Text -> UTCTime -> Maybe Int -> Maybe Text -> (Domain.VerificationStatus, Maybe Vehicle.Variant)
validateRCStatus mbVariant rcconfigs rcInsurenceConfigs expiry insuranceValidity cov now capacity manufacturer = do
  case mbVariant of
    Just variant -> (Domain.VALID, Just variant)
    Nothing -> do
      case rcconfigs.supportedVehicleClasses of
        ODC.RCValidClasses [] -> (Domain.INVALID, Nothing)
        ODC.RCValidClasses vehicleClassVariantMap -> do
          let validCOVsCheck = rcconfigs.vehicleClassCheckType
          let (isCOVValid, variant) = maybe (False, Nothing) (isValidCOVRC capacity manufacturer vehicleClassVariantMap validCOVsCheck) cov
          let validInsurance = (not rcInsurenceConfigs.checkExpiry) || maybe False (now <) insuranceValidity
          if ((not rcconfigs.checkExpiry) || now < expiry) && isCOVValid && validInsurance then (Domain.VALID, variant) else (Domain.INVALID, variant)
        _ -> (Domain.INVALID, Nothing)

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Maybe Int -> Maybe Text -> [ODC.VehicleClassVariantMap] -> ODC.VehicleClassCheckType -> Text -> (Bool, Maybe Vehicle.Variant)
isValidCOVRC capacity manufacturer vehicleClassVariantMap validCOVsCheck cov = do
  let vehicleClassVariant = find checkIfMatch vehicleClassVariantMap
  case vehicleClassVariant of
    Just obj -> (True, Just obj.vehicleVariant)
    Nothing -> (False, Nothing)
  where
    checkIfMatch obj = do
      let classMatched = classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) (T.toUpper cov)
      let capacityMatched = capacityCheckFunction obj.vehicleCapacity capacity
      let manufacturerMatched = manufacturerCheckFunction obj.manufacturer manufacturer
      classMatched && capacityMatched && manufacturerMatched

-- capacityCheckFunction validCapacity rcCapacity
capacityCheckFunction :: Maybe Int -> Maybe Int -> Bool
capacityCheckFunction (Just a) (Just b) = a == b
capacityCheckFunction Nothing (Just _) = True
capacityCheckFunction Nothing Nothing = True
capacityCheckFunction _ _ = False

manufacturerCheckFunction :: Maybe Text -> Maybe Text -> Bool
manufacturerCheckFunction (Just a) (Just b) = T.isInfixOf (T.toUpper a) (T.toUpper b)
manufacturerCheckFunction Nothing (Just _) = True
manufacturerCheckFunction Nothing Nothing = True
manufacturerCheckFunction _ _ = False

classCheckFunction :: ODC.VehicleClassCheckType -> Text -> Text -> Bool
classCheckFunction validCOVsCheck =
  case validCOVsCheck of
    ODC.Infix -> T.isInfixOf
    ODC.Prefix -> T.isPrefixOf
    ODC.Suffix -> T.isSuffixOf

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

convertUTCTimetoDate :: UTCTime -> Text
convertUTCTimetoDate utctime = T.pack (DT.formatTime DT.defaultTimeLocale "%d/%m/%Y" utctime)

rcVerificationLockKey :: Text -> Text
rcVerificationLockKey rcNumber = "VehicleRC::RCNumber-" <> rcNumber

makeFleetOwnerKey :: Text -> Text
makeFleetOwnerKey vehicleNo = "FleetOwnerId:PersonId-" <> vehicleNo
