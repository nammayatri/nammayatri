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
    RCLinkStatusReq (..),
    verifyRC,
    onVerifyRC,
    convertUTCTimetoDate,
    activateRC,
    linkRCStatus,
    inactivateRC,
    deleteRc,
    getAllRcData,
    LinkedRCs (..),
    DeleteRCReq (..),
  )
where

import AWS.S3 as S3
import Control.Applicative ((<|>))
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
import Domain.Types.Vehicle.Variant
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude hiding (find)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.DriverInformation as DriverInfo
import qualified Storage.CachedQueries.Merchant.OnboardingDocumentConfig as SCO
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import Storage.Queries.DriverInformation as DIQuery
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
    dateOfRegistration :: Maybe UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

data LinkedRCs = LinkedRCs
  { rcDetails :: Domain.DecryptedVehicleRegistrationCertificate,
    rcActive :: Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype DeleteRCReq = DeleteRCReq
  { rcNo :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data RCLinkStatusReq = RCLinkStatusReq
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
  Flow DriverRCRes
verifyRC isDashboard mbMerchant (personId, _) req@DriverRCReq {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  onboardingDocumentConfig <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RC))
  runRequestValidation (validateDriverRCReq onboardingDocumentConfig.rcNumberPrefix) req
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
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

  totalRcs <- DAQuery.findNumberOfRcs personId
  unless (length totalRcs < transporterConfig.rcLimit) $ throwError RCLimitReached

  checkForRCDuplicacy personId vehicleRegistrationCertNumber
  verifyRCFlow person onboardingDocumentConfig.checkExtraction vehicleRegistrationCertNumber imageId dateOfRegistration

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

checkForRCDuplicacy :: Id Person.Person -> Text -> Flow ()
checkForRCDuplicacy driverId vehicleRegistrationCertNumber = do
  mVehicleRC <- RCQuery.findLastVehicleRC vehicleRegistrationCertNumber
  case mVehicleRC of
    Just vehicleRC -> do
      mRCAssociation <- DAQuery.findByRCIdAndDriverId vehicleRC.id driverId
      when (isJust mRCAssociation) $ throwError DriverAlreadyLinked
    Nothing -> return ()

verifyRCFlow :: Person.Person -> Bool -> Text -> Id Image.Image -> Maybe UTCTime -> Flow ()
verifyRCFlow person imageExtraction rcNumber imageId dateOfRegistration = do
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
  runTransaction $ IVQuery.create idfyVerificationEntity
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
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Domain.IdfyVerification -> Idfy.RCVerificationOutput -> Flow AckResponse
onVerifyRC verificationReq output = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  if verificationReq.imageExtractionValidation == Domain.Skipped
    && isJust verificationReq.issueDateOnDoc
    && ( (convertUTCTimetoDate <$> verificationReq.issueDateOnDoc)
           /= (convertUTCTimetoDate <$> (convertTextToUTC output.registration_date))
       )
    then runTransaction $ IVQuery.updateExtractValidationStatus verificationReq.requestId Domain.Failed >> return Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      rCConfigs <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RC >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RC))
      rCInsuranceConfigs <- SCO.findByMerchantIdAndDocumentType person.merchantId ODC.RCInsurance >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show ODC.RCInsurance))
      mEncryptedRC <- encrypt `mapM` output.registration_number
      let mbFitnessEpiry = convertTextToUTC output.fitness_upto <|> Just (DT.UTCTime (TO.fromOrdinalDate 1900 1) 0)
      let mVehicleRC = createRC rCConfigs rCInsuranceConfigs output id verificationReq.documentImageId1 now <$> mEncryptedRC <*> mbFitnessEpiry
      case mVehicleRC of
        Just vehicleRC -> do
          runTransaction $ RCQuery.upsert vehicleRC
          -- linking to driver
          rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (InternalError "RC not found")
          driverRCAssoc <- mkAssociation person.id rc.id
          runTransaction $ DAQuery.create driverRCAssoc
          return Ack
        _ -> return Ack
  where
    mkAssociation driverId rcId = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.DriverRCAssociation
          { id,
            driverId,
            rcId,
            associatedOn = now,
            associatedTill = convertTextToUTC (Just "2099-12-12"),
            consent = True,
            consentTimestamp = now,
            isRcActive = False,
            isDeleted = False
          }

linkRCStatus :: (Id Person.Person, Id DM.Merchant) -> RCLinkStatusReq -> Flow APISuccess
linkRCStatus (driverId, merchantId) req@RCLinkStatusReq {..} = do
  if req.isActivate
    then do
      activateRC (driverId, merchantId) rcNo
      return Success
    else inactivateRC (driverId, merchantId) rcNo

inactivateRC :: (Id Person.Person, Id DM.Merchant) -> Text -> Flow APISuccess
inactivateRC (pId, _) rcNo = do
  isOnRide <- DIQuery.findByDriverIdActiveRide (cast pId)
  when (isJust isOnRide) $ throwError (InvalidRequest "Vehicle on ride. try again later.")
  rc <- RCQuery.findLastVehicleRC rcNo >>= fromMaybeM (InvalidRequest "Rc not found")
  Esq.runNoTransaction $ DAQuery.updateIsRcActiveByRcAndDriver pId rc.id
  Esq.runNoTransaction $ VQuery.deleteById pId
  return Success

inactiveOldRC :: Id Person.Person -> Flow ()
inactiveOldRC driverId = do
  Esq.runNoTransaction $ VQuery.deleteById driverId
  Esq.runNoTransaction $ DAQuery.findByDriverIdAndUpdateIsRcActive driverId

activateRC :: (Id Person.Person, Id DM.Merchant) -> Text -> Flow APISuccess
activateRC (driverId, merchantId) vehicleRegistrationCertNumber = do
  rc <- RCQuery.findLastVehicleRC vehicleRegistrationCertNumber >>= fromMaybeM (InvalidRequest "Rc not found")
  mactiveRC <- DAQuery.findActiveAssociationByRC rc.id
  case mactiveRC of
    Just activeRC -> do
      isOnRide <- DIQuery.findByDriverIdActiveRide (cast activeRC.driverId)
      when (isJust isOnRide) $ throwError (InvalidRequest "Vehicle on ride. try again later.")
      lastRideAssignedOn <- RQuery.findLastRideAssigned activeRC.driverId >>= fromMaybeM (PersonNotFound activeRC.driverId.getId)
      now <- getCurrentTime
      if abs (diffUTCTime now lastRideAssignedOn.createdAt) > 5 * 24 * 60 * 60
        then do
          Esq.runNoTransaction $ DAQuery.findByDriverIdAndUpdateIsRcActive lastRideAssignedOn.driverId
          let vehicle = buildVehicle now driverId merchantId vehicleRegistrationCertNumber rc
          Esq.runTransaction $ VQuery.upsert vehicle
          inactiveOldRC driverId
          return Success
        else throwError $ InvalidRequest "Rc Active on another driver account"
    Nothing -> do
      now <- getCurrentTime
      let vehicle = buildVehicle now driverId merchantId vehicleRegistrationCertNumber rc
      inactiveOldRC driverId
      Esq.runTransaction $ VQuery.upsert vehicle
      Esq.runTransaction $ DAQuery.updateIsRcAndDriverIdActiveByRc rc.id driverId
      return Success
  where
    buildVehicle now personId_ merchantId_ certificateNumber rc =
      Vehicle.Vehicle
        { Vehicle.driverId = personId_,
          Vehicle.capacity = rc.vehicleCapacity,
          Vehicle.category = Vehicle.getCategory <$> rc.vehicleVariant,
          Vehicle.make = rc.vehicleManufacturer,
          Vehicle.model = fromMaybe "Unkown" rc.vehicleModel,
          Vehicle.size = Nothing,
          Vehicle.merchantId = merchantId_,
          Vehicle.variant = fromMaybe AUTO_RICKSHAW rc.vehicleVariant, -- Value will be always Just if reaching here
          Vehicle.color = fromMaybe "Unkown" rc.vehicleColor,
          Vehicle.energyType = rc.vehicleEnergyType,
          Vehicle.registrationNo = certificateNumber,
          Vehicle.registrationCategory = Nothing,
          Vehicle.vehicleClass = fromMaybe "Unkown" rc.vehicleClass,
          Vehicle.createdAt = now,
          Vehicle.updatedAt = now
        }

deleteRc :: (Id Person.Person, Id DM.Merchant) -> DeleteRCReq -> Flow APISuccess
deleteRc (driverId, _) DeleteRCReq {..} = do
  rc <- RCQuery.findLastVehicleRC rcNo >>= fromMaybeM (InvalidRequest "Rc not found")
  Esq.runNoTransaction $ DAQuery.updateByDriverIdAndRCIdisDeleted driverId rc.id
  return Success

getAllRcData :: (Id Person.Person, Id DM.Merchant) -> Flow [LinkedRCs]
getAllRcData (driverId, _) = do
  rcAssociations <- DAQuery.findNumberOfRcs driverId
  decryptedRcsData <- mapM decrypt =<< RCQuery.findAllById (map (.rcId) rcAssociations)
  let activeRcs = buildRcHM rcAssociations
  return $ map (getCombinedRcData activeRcs) decryptedRcsData
  where
    getCombinedRcData activeRcs Domain.VehicleRegistrationCertificate {..} =
      LinkedRCs
        { rcActive = fromMaybe False $ HM.lookup id.getId activeRcs <&> (.isRcActive),
          rcDetails = Domain.VehicleRegistrationCertificate {..}
        }

createRC ::
  ODC.OnboardingDocumentConfig ->
  ODC.OnboardingDocumentConfig ->
  Idfy.RCVerificationOutput ->
  Id Domain.VehicleRegistrationCertificate ->
  Id Image.Image ->
  UTCTime ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.VehicleRegistrationCertificate
createRC rcconfigs rcInsurenceConfigs output id imageId now edl expiry = do
  let insuranceValidity = convertTextToUTC output.insurance_validity
  let vehicleClass = output.vehicle_class
  let vehicleCapacity = (readMaybe . T.unpack) =<< output.seating_capacity
  let (verificationStatus, variant) = validateRCStatus rcconfigs rcInsurenceConfigs expiry insuranceValidity vehicleClass now vehicleCapacity
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
      vehicleModel = output.m_y_manufacturing <|> output.manufacturer_model,
      vehicleColor = output.color <|> output.colour,
      vehicleEnergyType = output.fuel_type,
      insuranceValidity,
      verificationStatus,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }

validateRCStatus :: ODC.OnboardingDocumentConfig -> ODC.OnboardingDocumentConfig -> UTCTime -> Maybe UTCTime -> Maybe Text -> UTCTime -> Maybe Int -> (Domain.VerificationStatus, Maybe Vehicle.Variant)
validateRCStatus rcconfigs rcInsurenceConfigs expiry insuranceValidity cov now capacity = do
  case rcconfigs.supportedVehicleClasses of
    ODC.RCValidClasses [] -> (Domain.INVALID, Nothing)
    ODC.RCValidClasses vehicleClassVariantMap -> do
      let validCOVsCheck = rcconfigs.vehicleClassCheckType
      let (isCOVValid, variant) = maybe (False, Nothing) (isValidCOVRC capacity vehicleClassVariantMap validCOVsCheck) cov
      let validInsurance = (not rcInsurenceConfigs.checkExpiry) || maybe False (now <) insuranceValidity
      if ((not rcconfigs.checkExpiry) || now < expiry) && isCOVValid && validInsurance then (Domain.VALID, variant) else (Domain.INVALID, variant)
    _ -> (Domain.INVALID, Nothing)

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Maybe Int -> [ODC.VehicleClassVariantMap] -> ODC.VehicleClassCheckType -> Text -> (Bool, Maybe Vehicle.Variant)
isValidCOVRC capacity vehicleClassVariantMap validCOVsCheck cov = do
  let vehicleClassVariant = find checkIfMatch vehicleClassVariantMap
  case vehicleClassVariant of
    Just obj -> (True, Just obj.vehicleVariant)
    Nothing -> (False, Nothing)
  where
    checkIfMatch obj = do
      let classMatched = classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) (T.toUpper cov)
      let capacityMatched = capacityCheckFunction obj.vehicleCapacity capacity
      classMatched && capacityMatched

-- capacityCheckFunction validCapacity rcCapacity
capacityCheckFunction :: Maybe Int -> Maybe Int -> Bool
capacityCheckFunction (Just a) (Just b) = a == b
capacityCheckFunction Nothing (Just _) = True
capacityCheckFunction Nothing Nothing = True
capacityCheckFunction _ _ = False

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
