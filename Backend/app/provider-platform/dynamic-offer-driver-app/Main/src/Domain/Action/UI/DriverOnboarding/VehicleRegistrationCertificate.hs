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
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import Data.Text as T hiding (elem, find, length, map, null, zip)
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.IdfyVerification as DIV
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Vehicle as Vehicle
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
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
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as DIQuery
import Storage.Queries.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Storage.Queries.Ride as RQuery
import qualified Storage.Queries.Vehicle as VQuery
import qualified Storage.Queries.VehicleDetails as CQVD
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error
import qualified Tools.Verification as Verification

data DriverVehicleDetails = DriverVehicleDetails
  { vehicleManufacturer :: Text,
    vehicleModel :: Text,
    vehicleColour :: Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int
  }
  deriving (Generic, ToSchema, Show, ToJSON, FromJSON)

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe Vehicle.Category,
    airConditioned :: Maybe Bool,
    multipleRC :: Maybe Bool,
    vehicleDetails :: Maybe DriverVehicleDetails
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, _, merchantOpCityId) req = do
  runRequestValidation validateDriverRCReq req
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  documentVerificationConfig <- SCO.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId ODC.VehicleRegistrationCertificate (fromMaybe Vehicle.CAR req.vehicleCategory) >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.VehicleRegistrationCertificate))
  let checkPrefixOfRCNumber = prefixMatchedResult req.vehicleRegistrationCertNumber documentVerificationConfig.rcNumberPrefixList
  unless checkPrefixOfRCNumber $ throwError (InvalidRequest "RC number prefix is not valid")
  runRequestValidation validateDriverRCReq req
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just personId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  allLinkedRCs <- DAQuery.findAllLinkedByDriverId personId
  unless (length allLinkedRCs < (transporterConfig.rcLimit + (if isDashboard then 1 else 0))) $ throwError (RCLimitReached transporterConfig.rcLimit)

  let mbAirConditioned = maybe req.airConditioned (\category -> if category == Vehicle.CAR then req.airConditioned else Just False) req.vehicleCategory
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
  Redis.whenWithLockRedis (rcVerificationLockKey req.vehicleRegistrationCertNumber) 60 $ do
    whenJust mVehicleRC $ \vehicleRC -> do
      when (isNothing req.multipleRC) $ checkIfVehicleAlreadyExists person.id vehicleRC -- backward compatibility
    case req.vehicleDetails of
      Just DriverVehicleDetails {..} -> do
        vehicleDetails <- CQVD.findByMakeAndModel vehicleManufacturer vehicleModel
        void $ onVerifyRCHandler person (buildRCVerificationResponse vehicleDetails vehicleColour) req.vehicleCategory mbAirConditioned req.imageId ((vehicleDetails <&> (.vehicleVariant)) <|> Just Vehicle.HATCHBACK) vehicleDoors vehicleSeatBelts
      Nothing -> verifyRCFlow person merchantOpCityId documentVerificationConfig.checkExtraction req.vehicleRegistrationCertNumber req.imageId req.dateOfRegistration req.multipleRC req.vehicleCategory mbAirConditioned
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

    buildRCVerificationResponse vehicleDetails vehicleColour =
      Verification.RCVerificationResponse
        { registrationDate = show <$> req.dateOfRegistration,
          registrationNumber = Just req.vehicleRegistrationCertNumber,
          fitnessUpto = Nothing,
          insuranceValidity = Nothing,
          vehicleClass = Nothing,
          vehicleCategory = Nothing,
          seatingCapacity = (Just . String . show) <$> (.capacity) =<< vehicleDetails,
          manufacturer = vehicleDetails <&> (.make),
          permitValidityFrom = Nothing,
          permitValidityUpto = Nothing,
          pucValidityUpto = Nothing,
          manufacturerModel = vehicleDetails <&> (.model),
          mYManufacturing = Nothing,
          colour = Just vehicleColour,
          color = Just vehicleColour,
          fuelType = Nothing,
          bodyType = Nothing,
          status = Nothing
        }

verifyRCFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Id Image.Image -> Maybe UTCTime -> Maybe Bool -> Maybe Vehicle.Category -> Maybe Bool -> Flow ()
verifyRCFlow person merchantOpCityId imageExtraction rcNumber imageId dateOfRegistration multipleRC mbVehicleCategory mbAirConditioned = do
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
            vehicleCategory = mbVehicleCategory,
            airConditioned = mbAirConditioned,
            retryCount = Just 0,
            nameOnCard = Nothing,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Person.Person -> Maybe Domain.IdfyVerification -> VT.RCVerificationResponse -> Flow AckResponse
onVerifyRC person mbVerificationReq rcVerificationResponse = do
  if maybe False (\req -> req.imageExtractionValidation == Domain.Skipped && compareRegistrationDates rcVerificationResponse.registrationDate req.issueDateOnDoc) mbVerificationReq
    then IVQuery.updateExtractValidationStatus Domain.Failed (maybe "" (.requestId) mbVerificationReq) >> return Ack
    else do
      let mbVehicleCategory = mbVerificationReq >>= (.vehicleCategory)
      let mbAirConditioned = mbVerificationReq >>= (.airConditioned)
      void $ onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned (maybe "" (.documentImageId1) mbVerificationReq) Nothing Nothing Nothing
      return Ack

onVerifyRCHandler :: Person.Person -> VT.RCVerificationResponse -> Maybe Vehicle.Category -> Maybe Bool -> Id Image.Image -> Maybe Vehicle.Variant -> Maybe Int -> Maybe Int -> Flow ()
onVerifyRCHandler person rcVerificationResponse mbVehicleCategory mbAirConditioned mbDocumentImageId mbVehicleVariant mbVehicleDoors mbVehicleSeatBelts = do
  mbFleetOwnerId <- maybe (pure Nothing) (Redis.safeGet . makeFleetOwnerKey) rcVerificationResponse.registrationNumber
  now <- getCurrentTime
  let rcInput = createRCInput mbVehicleCategory mbFleetOwnerId mbDocumentImageId
  mVehicleRC <- do
    case mbVehicleVariant of
      Just vehicleVariant ->
        maybeM
          (return Nothing)
          ((Just <$>) . createVehicleRC person.merchantId person.merchantOperatingCityId rcInput vehicleVariant)
          (encrypt `mapM` rcVerificationResponse.registrationNumber)
      Nothing -> buildRC person.merchantId person.merchantOperatingCityId rcInput
  case mVehicleRC of
    Just vehicleRC -> do
      RCQuery.upsert vehicleRC
      -- linking to driver
      rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (RCNotFound (fromMaybe "" rcVerificationResponse.registrationNumber))
      mbAssoc <- DAQuery.findLinkedByRCIdAndDriverId person.id rc.id now
      when (isNothing mbAssoc) $ do
        driverRCAssoc <- makeRCAssociation person.merchantId person.merchantOperatingCityId person.id rc.id (convertTextToUTC (Just "2099-12-12"))
        DAQuery.create driverRCAssoc
      -- update vehicle details too if exists
      mbVehicle <- VQuery.findByRegistrationNo =<< decrypt rc.certificateNumber
      whenJust mbVehicle $ \vehicle -> do
        when (rc.verificationStatus == Domain.VALID && isJust rc.vehicleVariant) $ do
          driverInfo <- DIQuery.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
          driver <- Person.findById vehicle.driverId >>= fromMaybeM (PersonNotFound vehicle.driverId.getId)
          vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId
          let updatedVehicle = makeFullVehicleFromRC vehicleServiceTiers driverInfo driver person.merchantId vehicle.registrationNo rc person.merchantOperatingCityId now
          VQuery.upsert updatedVehicle
      whenJust rcVerificationResponse.registrationNumber $ \num -> Redis.del $ makeFleetOwnerKey num
    Nothing -> pure ()
  where
    createRCInput :: Maybe Vehicle.Category -> Maybe Text -> Id Image.Image -> CreateRCInput
    createRCInput vehicleCategory fleetOwnerId documentImageId =
      CreateRCInput
        { registrationNumber = rcVerificationResponse.registrationNumber,
          fitnessUpto = convertTextToUTC rcVerificationResponse.fitnessUpto,
          fleetOwnerId,
          vehicleCategory,
          airConditioned = mbAirConditioned,
          documentImageId,
          vehicleClass = rcVerificationResponse.vehicleClass,
          vehicleClassCategory = rcVerificationResponse.vehicleCategory,
          insuranceValidity = convertTextToUTC rcVerificationResponse.insuranceValidity,
          seatingCapacity = (readMaybe . T.unpack) =<< readFromJson =<< rcVerificationResponse.seatingCapacity,
          permitValidityUpto = convertTextToUTC rcVerificationResponse.permitValidityUpto,
          pucValidityUpto = convertTextToUTC rcVerificationResponse.pucValidityUpto,
          manufacturer = rcVerificationResponse.manufacturer,
          manufacturerModel = rcVerificationResponse.manufacturerModel,
          bodyType = rcVerificationResponse.bodyType,
          fuelType = rcVerificationResponse.fuelType,
          color = rcVerificationResponse.color <|> rcVerificationResponse.colour
        }

    readFromJson (String val) = Just val
    readFromJson (Number val) = Just $ T.pack $ show (floor val :: Int)
    readFromJson _ = Nothing

    createVehicleRC :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> CreateRCInput -> Vehicle.Variant -> EncryptedHashedField 'AsEncrypted Text -> Flow DVRC.VehicleRegistrationCertificate
    createVehicleRC merchantId merchantOperatingCityId input vehicleVariant certificateNumber = do
      now <- getCurrentTime
      id <- generateGUID
      return $
        DVRC.VehicleRegistrationCertificate
          { id,
            documentImageId = input.documentImageId,
            certificateNumber,
            fitnessExpiry = addUTCTime (secondsToNominalDiffTime 788400000) now, -- TODO :: Please fix me, if my usage is critical. I am hardcoded for next 50 years.
            permitExpiry = input.permitValidityUpto,
            pucExpiry = input.pucValidityUpto,
            vehicleClass = input.vehicleClass,
            vehicleVariant = Just vehicleVariant,
            vehicleManufacturer = input.manufacturer <|> input.manufacturerModel,
            vehicleCapacity = input.seatingCapacity,
            vehicleModel = input.manufacturerModel,
            vehicleColor = input.color,
            manufacturerModel = input.manufacturerModel,
            vehicleEnergyType = input.fuelType,
            reviewedAt = Nothing,
            reviewRequired = Nothing,
            insuranceValidity = input.insuranceValidity,
            verificationStatus = DIV.VALID,
            fleetOwnerId = input.fleetOwnerId,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            userPassedVehicleCategory = input.vehicleCategory,
            airConditioned = input.airConditioned,
            luggageCapacity = Nothing,
            vehicleRating = Nothing,
            failedRules = [],
            vehicleDoors = mbVehicleDoors,
            vehicleSeatBelts = mbVehicleSeatBelts,
            createdAt = now,
            updatedAt = now
          }

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

activateRC :: DI.DriverInformation -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Domain.VehicleRegistrationCertificate -> Flow ()
activateRC driverInfo merchantId merchantOpCityId now rc = do
  deactivateCurrentRC driverInfo.driverId
  addVehicleToDriver
  DAQuery.activateRCForDriver driverInfo.driverId rc.id now
  return ()
  where
    addVehicleToDriver = do
      rcNumber <- decrypt rc.certificateNumber
      transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId (Just driverInfo.driverId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      whenJust rc.vehicleVariant $ \variant -> do
        when (variant == Vehicle.SUV) $
          DIQuery.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi driverInfo.driverId
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
      person <- Person.findById driverInfo.driverId >>= fromMaybeM (PersonNotFound driverInfo.driverId.getId)
      let vehicle = makeFullVehicleFromRC cityVehicleServiceTiers driverInfo person merchantId rcNumber rc merchantOpCityId now
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
