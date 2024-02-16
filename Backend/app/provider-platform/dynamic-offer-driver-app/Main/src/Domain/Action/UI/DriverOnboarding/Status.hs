{-# LANGUAGE DerivingStrategies #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusHandler,
    mapStatus,
    verificationStatusWithMessage,
  )
where

-- temp import for backward compatibility should be removed later

import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.DriverOnboarding.AadhaarVerification as AV
import qualified Domain.Types.DriverOnboarding.DriverLicense as DL
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import Kernel.External.Encryption
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import SharedLogic.DriverOnboarding (enableAndTriggerOnboardingAlertsAndMessages)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import Storage.CachedQueries.Merchant.TransporterConfig
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as SAV
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as IQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as Person
import qualified Storage.Queries.Translations as MTQuery

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- INVALID is the state
--   which the doc switches to when, for example, it's expired.
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    dlVerficationMessage :: Text,
    rcVerificationStatus :: ResponseStatus,
    rcVerficationMessage :: Text,
    aadhaarVerificationStatus :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Flow StatusRes
statusHandler (personId, merchantId, merchantOpCityId) multipleRC = do
  -- multipleRC flag is temporary to support backward compatibility
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  merchantOperatingCity <- SMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  (dlStatus, mDL, dlVerficationMessage) <- getDLAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit merchantOperatingCity.language
  (rcStatus, mRC, rcVerficationMessage) <- getRCAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit multipleRC merchantOperatingCity.language
  (aadhaarStatus, _) <- getAadhaarStatus personId

  when (rcStatus == VALID && isNothing multipleRC) $
    activateRCAutomatically personId merchantId merchantOpCityId mRC

  when (dlStatus == VALID && rcStatus == VALID && (aadhaarStatus == VALID || not transporterConfig.aadhaarVerificationRequired)) $ do
    enableDriver merchantOpCityId personId mDL
  return $ StatusRes {dlVerificationStatus = dlStatus, dlVerficationMessage = dlVerficationMessage, rcVerificationStatus = rcStatus, rcVerficationMessage = rcVerficationMessage, aadhaarVerificationStatus = aadhaarStatus}

getAadhaarStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe AV.AadhaarVerification)
getAadhaarStatus personId = do
  mAadhaarCard <- SAV.findByDriverId personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.isVerified
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Language -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverId merchantOpCityId onboardingTryLimit language = do
  mDriverLicense <- DLQuery.findByDriverId driverId
  (status, message) <-
    case mDriverLicense of
      Just driverLicense -> do
        let status = mapStatus driverLicense.verificationStatus
        msg <- verificationStatusCheck status language Image.DriverLicense
        return (status, msg)
      Nothing -> do
        (status, message) <- checkIfInVerification driverId merchantOpCityId onboardingTryLimit Image.DriverLicense language
        return (status, message)
  return (status, mDriverLicense, message)

getRCAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Maybe Bool -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
getRCAndStatus driverId merchantOpCityId onboardingTryLimit multipleRC language = do
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      (status, message) <- checkIfInVerification driverId merchantOpCityId onboardingTryLimit Image.VehicleRegistrationCertificate language
      return (status, Nothing, message)
    else do
      mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
      let vehicleRCs = catMaybes mVehicleRCs
      if isNothing multipleRC -- for backward compatibility
        then do
          let firstRC = listToMaybe vehicleRCs
          case firstRC of
            Just vehicleRC -> do
              let status = mapStatus vehicleRC.verificationStatus
              message <- verificationStatusCheck status language Image.VehicleRegistrationCertificate
              return (status, Just vehicleRC, message)
            Nothing -> do
              msg <- toVerificationMessage NoDcoumentFound language
              return (NO_DOC_AVAILABLE, Nothing, msg)
        else do
          let mValidVehicleRC = find (\rc -> rc.verificationStatus == IV.VALID) vehicleRCs
          case mValidVehicleRC of
            Just validVehicleRC -> do
              msg <- toVerificationMessage DocumentValid language
              return (VALID, Just validVehicleRC, msg)
            Nothing -> do
              let mVehicleRC = listToMaybe vehicleRCs
              case mVehicleRC of
                Just vehicleRC -> do
                  let status = mapStatus vehicleRC.verificationStatus
                  message <- verificationStatusCheck status language Image.VehicleRegistrationCertificate
                  return (status, Just vehicleRC, message)
                Nothing -> do
                  msg <- toVerificationMessage NoDcoumentFound language
                  return (NO_DOC_AVAILABLE, Nothing, msg)

mapStatus :: IV.VerificationStatus -> ResponseStatus
mapStatus = \case
  IV.PENDING -> PENDING
  IV.VALID -> VALID
  IV.INVALID -> INVALID

verificationStatusCheck :: ResponseStatus -> Language -> Image.ImageType -> Flow Text
verificationStatusCheck status language img = do
  case (status, img) of
    (INVALID, Image.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, Image.VehicleRegistrationCertificate) -> toVerificationMessage RCInvalid language
    _ -> toVerificationMessage DocumentValid language

checkIfInVerification :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Image.ImageType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverId merchantOpCityId onboardingTryLimit docType language = do
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType
  images <- IQuery.findRecentByPersonIdAndImageType driverId merchantOpCityId docType
  verificationStatusWithMessage onboardingTryLimit (length images) verificationReq language

verificationStatusWithMessage :: Int -> Int -> Maybe IV.IdfyVerification -> Language -> Flow (ResponseStatus, Text)
verificationStatusWithMessage onboardingTryLimit imagesNum verificationReq language =
  case verificationReq of
    Just req -> do
      if req.status == "pending"
        then do
          msg <- toVerificationMessage VerificationInProgress language
          return (PENDING, msg)
        else do
          message <- getMessageFromResponse language req.idfyResponse
          return (FAILED, message)
    Nothing -> do
      if imagesNum > onboardingTryLimit
        then do
          msg <- toVerificationMessage LimitExceed language
          return (LIMIT_EXCEED, msg)
        else do
          msg <- toVerificationMessage NoDcoumentFound language
          return (NO_DOC_AVAILABLE, msg)

getMessageFromResponse :: Language -> Maybe Text -> Flow Text
getMessageFromResponse language response = do
  case response of
    Just res
      | "id_not_found" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | "source_down" `T.isInfixOf` res -> toVerificationMessage VerificationInProgress language
      | "TIMEOUT" `T.isInfixOf` res -> toVerificationMessage VerficationFailed language
      | "BAD_REQUEST" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | otherwise -> toVerificationMessage Other language
    Nothing -> toVerificationMessage Other language

enableDriver :: Id DMOC.MerchantOperatingCity -> Id SP.Person -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing = return ()
enableDriver merchantOpCityId personId (Just dl) = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  unless driverInfo.enabled $ do
    enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId True
    whenJust dl.driverName $ \name -> Person.updateName personId name

activateRCAutomatically :: Id SP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe RC.VehicleRegistrationCertificate -> Flow ()
activateRCAutomatically _ _ _ Nothing = return ()
activateRCAutomatically personId merchantId merchantOpCityId (Just rc) = do
  rcNumber <- decrypt rc.certificateNumber
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantId, merchantOpCityId) rcStatusReq

data VerificationMessage
  = InvalidDocumentNumber
  | VerficationFailed
  | NoDcoumentFound
  | LimitExceed
  | DLInvalid
  | RCInvalid
  | DocumentValid
  | VerificationInProgress
  | Other
  deriving (Show, Eq, Ord)

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"
