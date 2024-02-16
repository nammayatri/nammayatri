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
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import Data.Text as T hiding (elem, find, length, map, null, zip)
import qualified Data.Time as DT
import qualified Data.Time.Calendar.OrdinalDate as TO
import qualified Domain.Types.DriverOnboarding.DriverRCAssociation as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as ODC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Vehicle as Vehicle
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Types as VT
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
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.FleetDriverAssociation as FDVAQ
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
  Maybe Vehicle.Variant -> -- in case hardcoded variant passed from dashboard, then ignore variant coming from IDFY
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, _, merchantOpCityId) req@DriverRCReq {..} mbVariant = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbFleetOwnerId <- Redis.safeGet $ makeFleetOwnerKey vehicleRegistrationCertNumber
  whenJust mbFleetOwnerId $ \fleetOwnerId -> do
    isFleetDriver <- FDVAQ.findByDriverIdAndFleetOwnerId personId fleetOwnerId
    case isFleetDriver of
      Nothing -> throwError (InvalidRequest "Driver is not part of this fleet, add this driver to the fleet before adding a vehicle with them")
      Just fleetDriver -> do
        unless fleetDriver.isActive $ throwError (InvalidRequest "Driver is not active with this fleet, add this driver to the fleet before adding a vehicle with them")
  onboardingDocumentConfig <- SCO.findByMerchantOpCityIdAndDocumentType merchantOpCityId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound merchantOpCityId.getId (show ODC.RC))
  let checkPrefixOfRCNumber = prefixMatchedResult vehicleRegistrationCertNumber onboardingDocumentConfig.rcNumberPrefixList
  unless checkPrefixOfRCNumber $ throwError (InvalidRequest "RC number prefix is not valid")
  runRequestValidation validateDriverRCReq req
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
  unless (length allLinkedRCs < transporterConfig.rcLimit) $ throwError (RCLimitReached transporterConfig.rcLimit)

  when
    ( isNothing dateOfRegistration && onboardingDocumentConfig.checkExtraction
        && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
    )
    $ do
      image <- getImage imageId
      resp <-
        Verification.extractRCImage person.merchantId merchantOpCityId $
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
            if (maybe True (now >) assoc.associatedTill)
              then verifyRCFlow person merchantOpCityId onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant
              else RCQuery.updateFleetOwnerId vehicleRC.id mbFleetOwnerId
          Nothing -> do
            verifyRCFlow person merchantOpCityId onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant
      Nothing ->
        verifyRCFlow person merchantOpCityId onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant

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

verifyRCFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe Bool -> Maybe Vehicle.Variant -> Flow ()
verifyRCFlow person merchantOpCityId imageExtraction rcNumber imageId dateOfRegistration multipleRC mbVariant = do
  now <- getCurrentTime
  encryptedRC <- encrypt rcNumber
  let imageExtractionValidation =
        if isNothing dateOfRegistration && imageExtraction
          then Domain.Success
          else Domain.Skipped
  verifyRes <-
    Verification.verifyRC person.merchantId
      merchantOpCityId
      Verification.VerifyRCReq {rcNumber = rcNumber, driverId = person.id.getId}
  case verifyRes of
    Verification.AsyncResp res -> do
      idfyVerificationEntity <- mkIdfyVerificationEntity res.requestId now imageExtractionValidation encryptedRC
      IVQuery.create idfyVerificationEntity
    Verification.SyncResp res -> do
      void $ onVerifyRC person Nothing res
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
            retryCount = 0,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Person.Person -> Maybe Domain.IdfyVerification -> VT.RCVerificationResponse -> Flow AckResponse
onVerifyRC person mbVerificationReq output = do
  if maybe False (\req -> req.imageExtractionValidation == Domain.Skipped && compareRegistrationDates output.registrationDate req.issueDateOnDoc) mbVerificationReq
    then IVQuery.updateExtractValidationStatus (maybe "" (.requestId) mbVerificationReq) Domain.Failed >> return Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      rCConfigs <- SCO.findByMerchantOpCityIdAndDocumentType person.merchantOperatingCityId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantOperatingCityId.getId (show ODC.RC))
      rCInsuranceConfigs <- SCO.findByMerchantOpCityIdAndDocumentType person.merchantOperatingCityId ODC.RCInsurance >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantOperatingCityId.getId (show ODC.RCInsurance))
      mEncryptedRC <- encrypt `mapM` output.registrationNumber
      modelNamesHashMap <- asks (.modelNamesHashMap)
      let mbFitnessEpiry = convertTextToUTC output.fitnessUpto <|> convertTextToUTC output.permitValidityUpto <|> Just (DT.UTCTime (TO.fromOrdinalDate 1900 1) 0)
      fleetOwnerId <- maybe (pure Nothing) (Redis.safeGet . makeFleetOwnerKey) output.registrationNumber
      let mVehicleRC = createRC rCConfigs rCInsuranceConfigs output id (maybe "" (.documentImageId1) mbVerificationReq) now ((.dashboardPassedVehicleVariant) =<< mbVerificationReq) fleetOwnerId modelNamesHashMap <$> mEncryptedRC <*> mbFitnessEpiry
      case mVehicleRC of
        Just vehicleRC -> do
          RCQuery.upsert vehicleRC
          -- linking to driver
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" output.registrationNumber))
          driverRCAssoc <- Domain.makeRCAssociation person.id rc.id (convertTextToUTC (Just "2099-12-12"))
          DAQuery.create driverRCAssoc
          whenJust output.registrationNumber $ \num -> Redis.del $ makeFleetOwnerKey num
          return Ack
        _ -> return Ack

compareRegistrationDates :: Maybe Text -> Maybe UTCTime -> Bool
compareRegistrationDates actualDate providedDate =
  isJust providedDate
    && ((convertUTCTimetoDate <$> providedDate) /= (convertUTCTimetoDate <$> convertTextToUTC actualDate))

linkRCStatus :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> RCStatusReq -> Flow APISuccess
linkRCStatus (driverId, merchantId, merchantOpCityId) req@RCStatusReq {..} = do
  driverInfo <- DIQuery.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (driverInfo.subscribed || transporterConfig.openMarketUnBlocked) $ throwError (RCActivationFailedPaymentDue driverId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  unless (rc.verificationStatus == Domain.VALID) $ throwError (InvalidRequest "Can't perform activate/inactivate operations on invalid RC!")
  now <- getCurrentTime
  if req.isActivate
    then do
      validateRCActivation driverId merchantOpCityId rc
      activateRC driverId merchantId merchantOpCityId now rc
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

validateRCActivation :: Id Person.Person -> Id DMOC.MerchantOperatingCity -> Domain.VehicleRegistrationCertificate -> Flow ()
validateRCActivation driverId merchantOpCityId rc = do
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
      transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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

activateRC :: Id Person.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Domain.VehicleRegistrationCertificate -> Flow ()
activateRC driverId merchantId merchantOpCityId now rc = do
  deactivateCurrentRC driverId
  addVehicleToDriver
  DAQuery.activateRCForDriver driverId rc.id now
  return ()
  where
    addVehicleToDriver = do
      rcNumber <- decrypt rc.certificateNumber
      transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == Vehicle.SUV) $
          DIQuery.updateDriverDowngradeTaxiForSuv driverId transporterConfig.canSuvDowngradeToTaxi
      let vehicle = Domain.makeVehicleFromRC now driverId merchantId rcNumber rc
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

deleteRC :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DeleteRCReq -> Bool -> Flow APISuccess
deleteRC (driverId, _, _) DeleteRCReq {..} isOldFlow = do
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  mAssoc <- DAQuery.findActiveAssociationByRC rc.id
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
            rcDetails = Domain.makeRCAPIEntity rc rcNo
          }

createRC ::
  ODC.OnboardingDocumentConfig ->
  ODC.OnboardingDocumentConfig ->
  VT.RCVerificationResponse ->
  Id Domain.VehicleRegistrationCertificate ->
  Id Image.Image ->
  UTCTime ->
  Maybe Vehicle.Variant ->
  Maybe Text ->
  HM.HashMap Text Text ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.VehicleRegistrationCertificate
createRC rcconfigs rcInsurenceConfigs output id imageId now mbVariant mbFleetOwnerId modelNamesHashMap edl expiry = do
  let insuranceValidity = convertTextToUTC output.insuranceValidity
  let vehicleClass = output.vehicleClass
  let vehicleCategory = output.vehicleCategory
  let vehicleCapacity = (readMaybe . T.unpack) =<< readFromJson =<< output.seatingCapacity
  let manufacturer = (readMaybe . T.unpack) =<< output.manufacturer
  let (verificationStatus, variant) = validateRCStatus mbVariant rcconfigs rcInsurenceConfigs expiry insuranceValidity vehicleClass vehicleCategory now vehicleCapacity manufacturer output.bodyType
  Domain.VehicleRegistrationCertificate
    { id,
      documentImageId = imageId,
      certificateNumber = edl,
      fitnessExpiry = expiry,
      permitExpiry = convertTextToUTC output.permitValidityUpto,
      pucExpiry = convertTextToUTC output.pucValidityUpto,
      vehicleClass,
      vehicleVariant = variant,
      vehicleManufacturer = output.manufacturer <|> output.manufacturerModel,
      vehicleCapacity,
      vehicleModel = updateModel =<< (output.manufacturerModel <|> output.mYManufacturing),
      vehicleColor = output.color <|> output.colour,
      vehicleEnergyType = output.fuelType,
      insuranceValidity,
      verificationStatus,
      fleetOwnerId = mbFleetOwnerId,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }
  where
    updateModel modelFromIdfy = Just . fromMaybe "" $ HM.lookup modelFromIdfy modelNamesHashMap
    readFromJson (A.String val) = Just val
    readFromJson (A.Number val) = Just $ T.pack $ show (floor val :: Int)
    readFromJson _ = Nothing

validateRCStatus :: Maybe Vehicle.Variant -> ODC.OnboardingDocumentConfig -> ODC.OnboardingDocumentConfig -> UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> UTCTime -> Maybe Int -> Maybe Text -> Maybe Text -> (Domain.VerificationStatus, Maybe Vehicle.Variant)
validateRCStatus mbVariant rcconfigs rcInsurenceConfigs expiry insuranceValidity cov vehicleCategory now capacity manufacturer bodyType = do
  case mbVariant of
    Just variant -> (Domain.VALID, Just variant)
    Nothing -> do
      case rcconfigs.supportedVehicleClasses of
        ODC.RCValidClasses [] -> (Domain.INVALID, Nothing)
        ODC.RCValidClasses vehicleClassVariantMap -> do
          let validCOVsCheck = rcconfigs.vehicleClassCheckType
          let (isCOVValid, variant) = maybe (False, Nothing) (isValidCOVRC vehicleCategory capacity manufacturer bodyType vehicleClassVariantMap validCOVsCheck) (cov <|> vehicleCategory)
          let validInsurance = (not rcInsurenceConfigs.checkExpiry) || maybe False (now <) insuranceValidity
          if ((not rcconfigs.checkExpiry) || now < expiry) && isCOVValid && validInsurance then (Domain.VALID, variant) else (Domain.INVALID, variant)
        _ -> (Domain.INVALID, Nothing)

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> [ODC.VehicleClassVariantMap] -> ODC.VehicleClassCheckType -> Text -> (Bool, Maybe Vehicle.Variant)
isValidCOVRC mVehicleCategory capacity manufacturer bodyType vehicleClassVariantMap validCOVsCheck cov = do
  let vehicleClassVariant = DL.find checkIfMatch vehicleClassVariantMap
  case vehicleClassVariant of
    Just obj -> (True, Just obj.vehicleVariant)
    Nothing -> (False, Nothing)
  where
    checkIfMatch obj = do
      let classMatched = classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) (T.toUpper cov)
      let categoryMatched = maybe False (classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) . T.toUpper) mVehicleCategory
      let capacityMatched = capacityCheckFunction obj.vehicleCapacity capacity
      let manufacturerMatched = manufacturerCheckFunction obj.manufacturer manufacturer
      let bodyTypeMatched = bodyTypeCheckFunction obj.bodyType bodyType
      (classMatched || categoryMatched) && capacityMatched && manufacturerMatched && bodyTypeMatched

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

bodyTypeCheckFunction :: Maybe Text -> Maybe Text -> Bool
bodyTypeCheckFunction (Just a) (Just b) = T.isInfixOf (T.toUpper a) (T.toUpper b)
bodyTypeCheckFunction Nothing (Just _) = True
bodyTypeCheckFunction Nothing Nothing = True
bodyTypeCheckFunction _ _ = False

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
