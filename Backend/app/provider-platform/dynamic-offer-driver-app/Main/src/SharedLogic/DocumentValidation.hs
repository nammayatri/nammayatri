{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DocumentValidation
  ( validateAndCheckDocument,
    DriverVehicleDetails (..),
    DriverRCReq (..),
    DriverDocument (..),
    DriverDLReq (..),
    validateDriverRCReq,
    validateDriverRCReqRegexFlow,
    validateDriverDLReq,
    validateDriverDLReqRegexFlow,
    isRCNumberFormatValid,
    isDLNumberFormatValid,
    getDriverDocumentInfo,
    normalizeDocumentNumber,
    getRegexRulesFromDocumentConfig,
    prefixMatchedResult,
    matchesRegexSafely,
    validateByRegex,
    convertVehicleDetails,
    convertToCommonVehicleDetails,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Data.List as DL
import qualified Data.Text as T
import Data.Time (nominalDay)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.Image as Domain
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import Environment
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation (Validate)
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation (runRequestValidation, validateField)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.DriverOnboarding (removeSpaceAndDash, throwImageError)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as FOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCExtra
import Text.Regex.Posix ((=~))
import Tools.Error
import qualified Tools.Utils as Utils

convertVehicleDetails :: Common.DriverVehicleDetails -> DriverVehicleDetails
convertVehicleDetails Common.DriverVehicleDetails {..} = DriverVehicleDetails {..}

convertToCommonVehicleDetails :: DriverVehicleDetails -> Common.DriverVehicleDetails
convertToCommonVehicleDetails DriverVehicleDetails {..} = Common.DriverVehicleDetails {..}

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
    imageId :: Id Domain.Image,
    imageId2 :: Maybe (Id Domain.Image),
    udinNumber :: Maybe Text,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe VehicleCategory,
    vehicleClass :: Maybe Text,
    airConditioned :: Maybe Bool,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    vehicleDetails :: Maybe DriverVehicleDetails,
    isRCImageValidated :: Maybe Bool,
    engineNumber :: Maybe Text,
    chassisNumber :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverDocument = DriverDocument
  { panNumber :: Maybe Text,
    aadhaarNumber :: Maybe Text,
    dlNumber :: Maybe Text,
    gstNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    vehicleCategory :: Maybe VehicleCategory,
    imageId1 :: Id Domain.Image,
    imageId2 :: Maybe (Id Domain.Image),
    dateOfIssue :: Maybe UTCTime,
    nameOnCard :: Maybe Text,
    nameOnCardFromSdk :: Maybe Text,
    requestId :: Maybe Text,
    sdkTransactionId :: Maybe Text,
    isDLImageValidated :: Maybe Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber P.vehicleRegistrationCertNumberRule]

validateDriverRCReqRegexFlow :: Validate DriverRCReq
validateDriverRCReqRegexFlow DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber (MinLength 1)]

-- Validation functions moved from DriverLicense
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

isDLNumberFormatValid :: DVC.DocumentVerificationConfig -> Text -> Flow Bool
isDLNumberFormatValid documentVerificationConfig normalizedDLNumber =
  validateByRegex "DL" documentVerificationConfig normalizedDLNumber (pure True)

-- Helper functions moved from VehicleRegistrationCertificate
prefixMatchedResult :: Text -> [Text] -> Bool
prefixMatchedResult rcNumber = DL.any (`T.isPrefixOf` rcNumber)

normalizeDocumentNumber :: Text -> Text
normalizeDocumentNumber = T.toUpper . removeSpaceAndDash

getRegexRulesFromDocumentConfig :: DVC.DocumentVerificationConfig -> [Text]
getRegexRulesFromDocumentConfig config = maybe [] (mapMaybe (.regexValidation)) config.documentFields

matchesRegexSafely :: Text -> Text -> Text -> Flow (Maybe Bool)
matchesRegexSafely documentType input regexPattern = do
  result <- try @_ @SomeException $ do
    let matched = (T.unpack input =~ T.unpack regexPattern :: Bool)
    matched `seq` pure matched
  case result of
    Right matched -> pure (Just matched)
    Left err -> do
      logError $ "Invalid regex in DocumentVerificationConfig for " <> documentType <> " validation: " <> regexPattern <> ", error: " <> show err
      pure Nothing

validateByRegex :: Text -> DVC.DocumentVerificationConfig -> Text -> Flow Bool -> Flow Bool
validateByRegex documentType config input fallback = do
  let regexRules = getRegexRulesFromDocumentConfig config
  if null regexRules
    then fallback
    else do
      regexResults <- mapM (matchesRegexSafely documentType input) regexRules
      let validRegexResults = mapMaybe (\x -> x) regexResults
      if null validRegexResults
        then fallback
        else pure (or validRegexResults)

isRCNumberFormatValid :: DVC.DocumentVerificationConfig -> Text -> Flow Bool
isRCNumberFormatValid documentVerificationConfig normalizedRCNumber = do
  let normalizedPrefixList = normalizeDocumentNumber <$> documentVerificationConfig.rcNumberPrefixList
      rcLength = T.length normalizedRCNumber
      isLegacyCertNumFormatValid =
        rcLength >= 5
          && rcLength <= 12
          && T.all (\ch -> (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == ',') normalizedRCNumber
      fallbackCheck =
        pure $
          (null normalizedPrefixList || prefixMatchedResult normalizedRCNumber normalizedPrefixList)
            && isLegacyCertNumFormatValid
  validateByRegex "RC" documentVerificationConfig normalizedRCNumber fallbackCheck

-- Define a common function to handle role-based decryption
getDriverDocumentInfo :: Person.Person -> Flow (Bool, DriverDocument)
getDriverDocumentInfo person = do
  case person.role of
    role | role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS] -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedGstNumber <- mapM decrypt res.gstNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber Nothing decryptedGstNumber)
    _ -> do
      res <- QDriverInfo.findById person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedDlNumber <- mapM decrypt res.dlNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber decryptedDlNumber Nothing)

validateAndCheckDocument ::
  DVC.DocumentType ->
  DVC.DocumentVerificationConfig ->
  Person.Person ->
  Common.DocumentSpecificData ->
  Bool ->
  Maybe (Id Person.Person) ->
  Maybe DI.DriverInformation ->
  Flow ()
validateAndCheckDocument docType config person docSpecificData isDashboard mbFleetOwnerId mbDriverInfo = do
  case (docType, docSpecificData) of
    (DVC.DriverLicense, Common.DLData dlData) -> do
      now <- getCurrentTime
      let driverLicenseNumber = dlData.driverLicenseNumber
      let regexRules = getRegexRulesFromDocumentConfig config
          hasRegexRules = not (null regexRules)
      if hasRegexRules
        then
          runRequestValidation (validateDriverDLReqRegexFlow now) $
            DriverDLReq
              { imageId1 = cast dlData.imageId1,
                imageId2 = cast <$> dlData.imageId2,
                vehicleCategory = dlData.vehicleCategory,
                driverLicenseNumber = dlData.driverLicenseNumber,
                operatingCity = dlData.operatingCity,
                driverDateOfBirth = dlData.driverDateOfBirth,
                dateOfIssue = dlData.dateOfIssue,
                nameOnCardFromSdk = Nothing,
                requestId = Nothing,
                sdkTransactionId = Nothing,
                nameOnCard = Nothing,
                isDLImageValidated = Nothing
              }
        else
          runRequestValidation (validateDriverDLReq now) $
            DriverDLReq
              { imageId1 = cast dlData.imageId1,
                imageId2 = cast <$> dlData.imageId2,
                vehicleCategory = dlData.vehicleCategory,
                driverLicenseNumber = dlData.driverLicenseNumber,
                operatingCity = dlData.operatingCity,
                driverDateOfBirth = dlData.driverDateOfBirth,
                dateOfIssue = dlData.dateOfIssue,
                nameOnCardFromSdk = Nothing,
                requestId = Nothing,
                sdkTransactionId = Nothing,
                nameOnCard = Nothing,
                isDLImageValidated = Nothing
              }
      driverInfo <- maybe (QDriverInfo.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)) pure mbDriverInfo
      when driverInfo.blocked $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)

      let normalizedDLNumber = normalizeDocumentNumber driverLicenseNumber
      checkDLFormat <- isDLNumberFormatValid config normalizedDLNumber
      unless checkDLFormat $
        throwError (InvalidRequest "DL number format is not valid")

      mdriverLicense <- DLQuery.findByDLNumber driverLicenseNumber
      case mdriverLicense of
        Just driverLicense -> do
          when (driverLicense.driverId /= person.id) $
            if fromMaybe False config.allowLicenseTransfer
              then do
                mDriverDL <- DLQuery.findByDriverId person.id
                whenJust mDriverDL $ \_ -> throwImageError (cast dlData.imageId1) DriverAlreadyLinked
              else do
                -- Fleet-aware duplicate check: single query for both drivers' fleet associations
                allAssocs <- QFDA.findAllByDriverIds [person.id, driverLicense.driverId]
                let existingFleetIds = [assoc.fleetOwnerId | assoc <- allAssocs, assoc.driverId == driverLicense.driverId]
                    targetFleetIds = [assoc.fleetOwnerId | assoc <- allAssocs, assoc.driverId == person.id]
                    sharedFleets = filter (`elem` existingFleetIds) targetFleetIds
                Utils.cleanupUploadedImages ([cast dlData.imageId1] <> maybe [] (pure . cast) dlData.imageId2) person.id
                unless (null sharedFleets) $ throwError DLAlreadyExistsInFleet
                when (driverLicense.verificationStatus == Documents.VALID && not (null existingFleetIds)) $
                  throwError DLLinkedToAnotherFleet
                throwImageError (cast dlData.imageId1) DLAlreadyLinked
          if fromMaybe False config.allowLicenseTransfer
            then pure ()
            else unless (driverLicense.licenseExpiry > now) $ throwImageError (cast dlData.imageId1) DLAlreadyUpdated
          transporterConfig <- getTransporterConfig person.merchantOperatingCityId
          when (driverLicense.verificationStatus == Documents.VALID && not (fromMaybe False config.allowLicenseTransfer) && not (fromMaybe False transporterConfig.allowDlReupload)) $ do
            Utils.cleanupUploadedImages ([cast dlData.imageId1] <> maybe [] (pure . cast) dlData.imageId2) person.id
            throwError $ DocumentAlreadyValidated "DL"
          when (config.doStrictVerifcation && driverLicense.verificationStatus == Documents.INVALID) $ throwError DLInvalid
        Nothing -> do
          mDriverDL <- DLQuery.findByDriverId person.id
          when (isJust mDriverDL) $ do
            Utils.cleanupUploadedImages ([cast dlData.imageId1] <> maybe [] (pure . cast) dlData.imageId2) person.id
            throwImageError (cast dlData.imageId1) DriverAlreadyLinked
    (DVC.VehicleRegistrationCertificate, Common.RCData rcData) -> do
      let vehicleRegistrationCertNumber = rcData.vehicleRegistrationCertNumber
          isTtenCertificate = isJust rcData.udinNumber
      (blocked, _) <- getDriverDocumentInfo person
      when blocked $ throwError AccountBlocked

      unless isTtenCertificate $ do
        let regexRules = getRegexRulesFromDocumentConfig config
            hasRegexRules = not (null regexRules)
        if hasRegexRules
          then
            runRequestValidation validateDriverRCReqRegexFlow $
              DriverRCReq
                { vehicleRegistrationCertNumber = rcData.vehicleRegistrationCertNumber,
                  imageId = cast rcData.imageId,
                  imageId2 = cast <$> rcData.imageId2,
                  udinNumber = rcData.udinNumber,
                  operatingCity = rcData.operatingCity,
                  dateOfRegistration = rcData.dateOfRegistration,
                  vehicleCategory = rcData.vehicleCategory,
                  vehicleClass = rcData.vehicleClass,
                  airConditioned = rcData.airConditioned,
                  oxygen = rcData.oxygen,
                  ventilator = rcData.ventilator,
                  vehicleDetails = convertVehicleDetails <$> rcData.vehicleDetails,
                  isRCImageValidated = Nothing,
                  engineNumber = rcData.engineNumber,
                  chassisNumber = rcData.chassisNumber
                }
          else
            runRequestValidation validateDriverRCReq $
              DriverRCReq
                { vehicleRegistrationCertNumber = rcData.vehicleRegistrationCertNumber,
                  imageId = cast rcData.imageId,
                  imageId2 = cast <$> rcData.imageId2,
                  udinNumber = rcData.udinNumber,
                  operatingCity = rcData.operatingCity,
                  dateOfRegistration = rcData.dateOfRegistration,
                  vehicleCategory = rcData.vehicleCategory,
                  vehicleClass = rcData.vehicleClass,
                  airConditioned = rcData.airConditioned,
                  oxygen = rcData.oxygen,
                  ventilator = rcData.ventilator,
                  vehicleDetails = convertVehicleDetails <$> rcData.vehicleDetails,
                  isRCImageValidated = Nothing,
                  engineNumber = rcData.engineNumber,
                  chassisNumber = rcData.chassisNumber
                }

        let normalizedRCNumber = normalizeDocumentNumber vehicleRegistrationCertNumber
        checkRCFormat <- isRCNumberFormatValid config normalizedRCNumber
        unless checkRCFormat $ do
          if hasRegexRules
            then throwError (InvalidRequest "RC number format is not valid")
            else throwError (InvalidRequest "RC number prefix is not valid")

      when (person.role == Person.DRIVER) $ do
        transporterConfig <- getTransporterConfig person.merchantOperatingCityId
        allLinkedRCs <- DAQuery.findAllLinkedByDriverId person.id
        rcs <- RCQuery.findAllById (map (.rcId) allLinkedRCs)
        let validLinkedRCs = filter (\rc -> rc.verificationStatus /= Documents.INVALID) rcs
        unless (length validLinkedRCs < (transporterConfig.rcLimit + (if isDashboard then 1 else 0))) $
          throwError (RCLimitReached transporterConfig.rcLimit)

      mbExistingRC <- VRCExtra.findLastVehicleRCWrapper vehicleRegistrationCertNumber
      whenJust mbExistingRC $ \existingRC -> do
        whenJust mbFleetOwnerId $ \fleetOwnerId ->
          whenJust existingRC.fleetOwnerId $ \existingFleetId ->
            when (existingFleetId /= fleetOwnerId.getId) $
              throwError VehicleBelongsToAnotherFleet
        when (existingRC.verificationStatus == Documents.VALID) $ do
          driverAssocs <- DAQuery.findAllLinkedByDriverId person.id
          unless (existingRC.id `elem` map (.rcId) driverAssocs) $
            throwError (InvalidRequest "RC with this vehicle number plate already exists")
    (DVC.PanCard, Common.PanData panData) -> do
      let panNumber = panData.panNumber
      (blocked, _) <- getDriverDocumentInfo person
      when blocked $ throwError AccountBlocked
      transporterConfig <- getTransporterConfig person.merchantOperatingCityId
      let inferPanType pan =
            let upperPan = T.toUpper pan
             in if T.length upperPan >= 4
                  then Just $ if T.index upperPan 3 == 'P' then DPan.INDIVIDUAL else DPan.BUSINESS
                  else Nothing
      when (transporterConfig.individualPANCheck == Just True && (person.role == Person.DRIVER || person.role == Person.FLEET_OWNER)) $
        when (inferPanType (removeSpaceAndDash panNumber) /= Just DPan.INDIVIDUAL) $
          throwError (InvalidRequest "Business PAN card not be accepted please upload individual PAN")
      case transporterConfig.allowDuplicatePan of
        Just False -> do
          panHash <- getDbHash panNumber
          panInfoList <- DPQuery.findAllByEncryptedPanNumber panHash
          let otherDriverIds = filter (/= person.id) (map (.driverId) panInfoList)
          unless (null otherDriverIds) $ do
            Utils.cleanupUploadedImages [cast panData.imageId] person.id
            throwError PanAlreadyLinked
          when (not (fromMaybe False transporterConfig.allowPanReupload)) $ do
            panPersonDetails <- QPerson.getDriversByIdIn (map (.driverId) panInfoList)
            let getRoles = map (.role) panPersonDetails
            when (person.role `elem` getRoles) $ throwError PanAlreadyLinked
        _ -> pure ()
      mdriverPanCard <- DPQuery.findByDriverId person.id
      whenJust mdriverPanCard $ \driverPanCard ->
        when (driverPanCard.verificationStatus == Documents.VALID && not (fromMaybe False transporterConfig.allowPanReupload)) $ do
          Utils.cleanupUploadedImages [cast panData.imageId] person.id
          throwError $ DocumentAlreadyValidated "PAN"
    (DVC.AadhaarCard, Common.AadhaarData aadhaarData) -> do
      (blocked, _) <- getDriverDocumentInfo person
      when blocked $ throwError AccountBlocked
      transporterConfig <- getTransporterConfig person.merchantOperatingCityId
      let imageIds = [cast aadhaarData.aadhaarFrontImageId] <> maybe [] (\b -> [cast b]) aadhaarData.aadhaarBackImageId
      case (transporterConfig.allowDuplicateAadhaar, aadhaarData.aadhaarNumber) of
        (Just False, Just aadhaarNumber) -> do
          aadhaarHash <- getDbHash aadhaarNumber
          aadhaarInfoList <- QAadhaarCard.findAllByEncryptedAadhaarNumber (Just aadhaarHash)
          let otherDriverIds = filter (/= person.id) (map (.driverId) aadhaarInfoList)
          unless (null otherDriverIds) $ do
            Utils.cleanupUploadedImages imageIds person.id
            throwError AadhaarAlreadyLinked
          when (not (fromMaybe False transporterConfig.allowAadhaarReupload)) $ do
            aadhaarPersonDetails <- QPerson.getDriversByIdIn (map (.driverId) aadhaarInfoList)
            let getRoles = map (.role) aadhaarPersonDetails
            when (person.role `elem` getRoles) $ throwError AadhaarAlreadyLinked
        _ -> pure ()
      when (not (fromMaybe False transporterConfig.allowAadhaarReupload)) $ do
        aadhaarInfo <- QAadhaarCard.findByPrimaryKey person.id
        whenJust aadhaarInfo $ \aadhaarInfoData ->
          when (aadhaarInfoData.verificationStatus == Documents.VALID) $ do
            Utils.cleanupUploadedImages imageIds person.id
            throwError $ DocumentAlreadyValidated "Aadhaar"
    _ -> do
      pure ()
  where
    getTransporterConfig merchantOpCityId = do
      getOneConfig
        (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
        Nothing
        >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
