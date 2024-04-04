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
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Vehicle as Vehicle
import qualified Domain.Types.VehicleRegistrationCertificate as Domain
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
import qualified Storage.CachedQueries.DocumentVerificationConfig as SCO
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.DriverInformation as DIQuery
import Storage.Queries.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociation as FDVAQ
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Storage.Queries.Ride as RQuery
import qualified Storage.Queries.Vehicle as VQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error
import qualified Tools.Verification as Verification

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe Vehicle.Category,
    multipleRC :: Maybe Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

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
  documentVerificationConfig <- SCO.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId ODC.VehicleRegistrationCertificate (fromMaybe Vehicle.CAR req.vehicleCategory) >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.VehicleRegistrationCertificate))
  let checkPrefixOfRCNumber = prefixMatchedResult vehicleRegistrationCertNumber documentVerificationConfig.rcNumberPrefixList
  unless checkPrefixOfRCNumber $ throwError (InvalidRequest "RC number prefix is not valid")
  runRequestValidation validateDriverRCReq req
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just personId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
  unless (length allLinkedRCs < transporterConfig.rcLimit) $ throwError (RCLimitReached transporterConfig.rcLimit)

  when
    ( isNothing dateOfRegistration && documentVerificationConfig.checkExtraction
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
          RCQuery.updateVehicleVariant vehicleRC.id mbVariant Nothing Nothing -- update vehicleVariant of RC is passed hardcoded from dashboard
        when (isNothing multipleRC) $ checkIfVehicleAlreadyExists person.id vehicleRC -- backward compatibility
        mRCAssociation <- DAQuery.findLatestByRCIdAndDriverId vehicleRC.id person.id
        case mRCAssociation of
          Just assoc -> do
            now <- getCurrentTime
            if (maybe True (now >) assoc.associatedTill)
              then verifyRCFlow person merchantOpCityId documentVerificationConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant req.vehicleCategory
              else RCQuery.updateFleetOwnerId mbFleetOwnerId vehicleRC.id
          Nothing -> do
            verifyRCFlow person merchantOpCityId documentVerificationConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant req.vehicleCategory
      Nothing ->
        verifyRCFlow person merchantOpCityId documentVerificationConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration multipleRC mbVariant req.vehicleCategory

  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId_ = do
      imageMetadata <- ImageQuery.findById imageId_ >>= fromMaybeM (ImageNotFound imageId_.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId_.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId_.getId)
      unless (imageMetadata.imageType == ODC.VehicleRegistrationCertificate) $
        throwError (ImageInvalidType (show ODC.VehicleRegistrationCertificate) (show imageMetadata.imageType))
      S3.get $ T.unpack imageMetadata.s3Path

verifyRCFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe Bool -> Maybe Vehicle.Variant -> Maybe Vehicle.Category -> Flow ()
verifyRCFlow person merchantOpCityId imageExtraction rcNumber imageId dateOfRegistration multipleRC mbVariant mbVehicleCategory = do
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
            docType = ODC.VehicleRegistrationCertificate,
            documentNumber = encryptedRC,
            driverDateOfBirth = Nothing,
            imageExtractionValidation = imageExtractionValidation,
            issueDateOnDoc = dateOfRegistration,
            status = "pending",
            idfyResponse = Nothing,
            multipleRC,
            dashboardPassedVehicleVariant = mbVariant,
            vehicleCategory = mbVehicleCategory,
            retryCount = Just 0,
            nameOnCard = Nothing,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Person.Person -> Maybe Domain.IdfyVerification -> VT.RCVerificationResponse -> Flow AckResponse
onVerifyRC person mbVerificationReq output = do
  if maybe False (\req -> req.imageExtractionValidation == Domain.Skipped && compareRegistrationDates output.registrationDate req.issueDateOnDoc) mbVerificationReq
    then IVQuery.updateExtractValidationStatus Domain.Failed (maybe "" (.requestId) mbVerificationReq) >> return Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      let mbVehicleCategory = mbVerificationReq >>= (.vehicleCategory)
      rCConfigs <- SCO.findByMerchantOpCityIdAndDocumentTypeAndCategory person.merchantOperatingCityId ODC.VehicleRegistrationCertificate (fromMaybe Vehicle.CAR mbVehicleCategory) >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show ODC.VehicleRegistrationCertificate))
      mEncryptedRC <- encrypt `mapM` output.registrationNumber
      modelNamesHashMap <- asks (.modelNamesHashMap)
      let mbFitnessEpiry = convertTextToUTC output.fitnessUpto <|> convertTextToUTC output.permitValidityUpto <|> Just (DT.UTCTime (TO.fromOrdinalDate 1900 1) 0)
      fleetOwnerId <- maybe (pure Nothing) (Redis.safeGet . makeFleetOwnerKey) output.registrationNumber
      let mVehicleRC = createRC person.merchantId person.merchantOperatingCityId mbVehicleCategory rCConfigs output id (maybe "" (.documentImageId1) mbVerificationReq) now ((.dashboardPassedVehicleVariant) =<< mbVerificationReq) fleetOwnerId modelNamesHashMap <$> mEncryptedRC <*> mbFitnessEpiry
      case mVehicleRC of
        Just vehicleRC -> do
          RCQuery.upsert vehicleRC
          -- linking to driver
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" output.registrationNumber))
          mbAssoc <- DAQuery.findLinkedByRCIdAndDriverId person.id rc.id now
          when (isNothing mbAssoc) $ do
            driverRCAssoc <- makeRCAssociation person.merchantId person.merchantOperatingCityId person.id rc.id (convertTextToUTC (Just "2099-12-12"))
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
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just driverId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (driverInfo.subscribed || transporterConfig.openMarketUnBlocked) $ throwError (RCActivationFailedPaymentDue driverId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  unless (rc.verificationStatus == Domain.VALID) $ throwError (InvalidRequest "Can't perform activate/inactivate operations on invalid RC!")
  now <- getCurrentTime
  if req.isActivate
    then do
      validated <- validateRCActivation driverId merchantOpCityId rc
      when validated $ activateRC driverId merchantId merchantOpCityId now rc
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
      if (activeAssociation.driverId == driverId)
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
      transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just oldDriverId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
      transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just driverId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == Vehicle.SUV) $
          DIQuery.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi driverId
      let vehicle = makeVehicleFromRC now driverId merchantId rcNumber rc merchantOpCityId
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

createRC ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Vehicle.Category ->
  ODC.DocumentVerificationConfig ->
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
createRC merchantId merchantOperatingCityId vehicleCategory rcconfigs output id imageId now mbVariant mbFleetOwnerId modelNamesHashMap edl expiry = do
  let insuranceValidity = convertTextToUTC output.insuranceValidity
  let vehicleClass = output.vehicleClass
  let vehicleClassCategory = output.vehicleCategory
  let vehicleCapacity = (readMaybe . T.unpack) =<< readFromJson =<< output.seatingCapacity
  let (verificationStatus, reviewRequired, variant, mbVehicleModel) = validateRCStatus mbVariant rcconfigs expiry insuranceValidity vehicleClass vehicleClassCategory now vehicleCapacity output.manufacturer output.bodyType output.manufacturerModel
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
      vehicleModel = mbVehicleModel <|> (updateModel =<< (output.manufacturerModel <|> output.mYManufacturing)),
      vehicleColor = output.color <|> output.colour,
      manufacturerModel = output.manufacturerModel,
      vehicleEnergyType = output.fuelType,
      reviewedAt = Nothing,
      reviewRequired,
      insuranceValidity,
      verificationStatus,
      fleetOwnerId = mbFleetOwnerId,
      merchantId = Just merchantId,
      merchantOperatingCityId = Just merchantOperatingCityId,
      userPassedVehicleCategory = vehicleCategory,
      airConditioned = Nothing,
      luggageCapacity = Nothing,
      vehicleRating = Nothing,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }
  where
    updateModel modelFromIdfy = Just . fromMaybe "" $ HM.lookup modelFromIdfy modelNamesHashMap
    readFromJson (A.String val) = Just val
    readFromJson (A.Number val) = Just $ T.pack $ show (floor val :: Int)
    readFromJson _ = Nothing

validateRCStatus :: Maybe Vehicle.Variant -> ODC.DocumentVerificationConfig -> UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> UTCTime -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> (Domain.VerificationStatus, Maybe Bool, Maybe Vehicle.Variant, Maybe Text)
validateRCStatus mbVariant rcconfigs expiry _insuranceValidity cov vehicleCategory now capacity manufacturer bodyType manufacturerModel = do
  case mbVariant of
    Just variant -> (Domain.VALID, Just False, Just variant, Nothing)
    Nothing -> do
      case rcconfigs.supportedVehicleClasses of
        ODC.RCValidClasses [] -> (Domain.INVALID, Nothing, Nothing, Nothing)
        ODC.RCValidClasses vehicleClassVariantMap -> do
          let validCOVsCheck = rcconfigs.vehicleClassCheckType
          let (isCOVValid, reviewRequired, variant, mbVehicleModel) = maybe (False, Nothing, Nothing, Nothing) (isValidCOVRC vehicleCategory capacity manufacturer bodyType manufacturerModel vehicleClassVariantMap validCOVsCheck) (cov <|> vehicleCategory)
          let validInsurance = True -- (not rcInsurenceConfigs.checkExpiry) || maybe False (now <) insuranceValidity
          if ((not rcconfigs.checkExpiry) || now < expiry) && isCOVValid && validInsurance then (Domain.VALID, reviewRequired, variant, mbVehicleModel) else (Domain.INVALID, reviewRequired, variant, mbVehicleModel)
        _ -> (Domain.INVALID, Nothing, Nothing, Nothing)

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> [ODC.VehicleClassVariantMap] -> ODC.VehicleClassCheckType -> Text -> (Bool, Maybe Bool, Maybe Vehicle.Variant, Maybe Text)
isValidCOVRC mVehicleCategory capacity manufacturer bodyType manufacturerModel vehicleClassVariantMap validCOVsCheck cov = do
  let sortedVariantMap = sortMaybe vehicleClassVariantMap
  let vehicleClassVariant = DL.find checkIfMatch sortedVariantMap
  case vehicleClassVariant of
    Just obj -> (True, obj.reviewRequired, Just obj.vehicleVariant, obj.vehicleModel)
    Nothing -> (False, Nothing, Nothing, Nothing)
  where
    checkIfMatch obj = do
      let classMatched = classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) (T.toUpper cov)
      let categoryMatched = maybe False (classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) . T.toUpper) mVehicleCategory
      let capacityMatched = capacityCheckFunction obj.vehicleCapacity capacity
      let manufacturerMatched = manufacturerCheckFunction obj.manufacturer manufacturer
      let manufacturerModelMatched = manufacturerModelCheckFunction obj.manufacturerModel manufacturerModel
      let bodyTypeMatched = bodyTypeCheckFunction obj.bodyType bodyType
      (classMatched || categoryMatched) && capacityMatched && manufacturerMatched && manufacturerModelMatched && bodyTypeMatched

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

manufacturerModelCheckFunction :: Maybe Text -> Maybe Text -> Bool
manufacturerModelCheckFunction (Just a) (Just b) = T.isInfixOf (T.toUpper a) (T.toUpper b)
manufacturerModelCheckFunction Nothing (Just _) = True
manufacturerModelCheckFunction Nothing Nothing = True
manufacturerModelCheckFunction _ _ = False

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

compareMaybe :: Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing Nothing = EQ
compareMaybe Nothing _ = GT
compareMaybe _ Nothing = LT
compareMaybe (Just x) (Just y) = compare x y

compareVehicles :: ODC.VehicleClassVariantMap -> ODC.VehicleClassVariantMap -> Ordering
compareVehicles a b =
  compareMaybe a.manufacturer b.manufacturer
    `mappend` compareMaybe a.manufacturerModel b.manufacturerModel
    `mappend` compareMaybe a.vehicleCapacity b.vehicleCapacity

-- Function to sort list of Maybe values
sortMaybe :: [ODC.VehicleClassVariantMap] -> [ODC.VehicleClassVariantMap]
sortMaybe = DL.sortBy compareVehicles
