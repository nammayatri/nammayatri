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
    verificationStatus,
  )
where

import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC -- temp import for backward compatibility should be removed later
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
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.CachedQueries.Merchant.TransporterConfig
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as SAV
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as IQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as Person

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- INVALID is the state
--   which the doc switches to when, for example, it's expired.
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus,
    aadhaarVerificationStatus :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Flow StatusRes
statusHandler (personId, merchantId, merchantOpCityId) multipleRC = do
  -- multipleRC flag is temporary to support backward compatibility
  transporterConfig <- findByMerchantOpCityId merchantOpCityId 0 Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  (dlStatus, mDL) <- getDLAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit
  (rcStatus, mRC) <- getRCAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit multipleRC
  (aadhaarStatus, _) <- getAadhaarStatus personId

  when (rcStatus == VALID && isNothing multipleRC) $
    activateRCAutomatically personId merchantId merchantOpCityId mRC

  when (dlStatus == VALID && rcStatus == VALID && (aadhaarStatus == VALID || not transporterConfig.aadhaarVerificationRequired)) $ do
    enableDriver personId mDL
  return $ StatusRes {dlVerificationStatus = dlStatus, rcVerificationStatus = rcStatus, aadhaarVerificationStatus = aadhaarStatus}

getAadhaarStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe AV.AadhaarVerification)
getAadhaarStatus personId = do
  mAadhaarCard <- SAV.findByDriverId personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.isVerified
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Flow (ResponseStatus, Maybe DL.DriverLicense)
getDLAndStatus driverId merchantOpCityId onboardingTryLimit = do
  mDriverLicense <- DLQuery.findByDriverId driverId
  status <-
    case mDriverLicense of
      Just driverLicense -> return $ mapStatus driverLicense.verificationStatus
      Nothing -> do
        checkIfInVerification driverId merchantOpCityId onboardingTryLimit Image.DriverLicense
  return (status, mDriverLicense)

getRCAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Maybe Bool -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate)
getRCAndStatus driverId merchantOpCityId onboardingTryLimit multipleRC = do
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      status <- checkIfInVerification driverId merchantOpCityId onboardingTryLimit Image.VehicleRegistrationCertificate
      return (status, Nothing)
    else do
      mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
      let vehicleRCs = catMaybes mVehicleRCs
      if isNothing multipleRC -- for backward compatibility
        then do
          let firstRC = listToMaybe vehicleRCs
          case firstRC of
            Just vehicleRC -> return (mapStatus vehicleRC.verificationStatus, Just vehicleRC)
            Nothing -> return (NO_DOC_AVAILABLE, Nothing)
        else do
          let mValidVehicleRC = find (\rc -> rc.verificationStatus == IV.VALID) vehicleRCs
          case mValidVehicleRC of
            Just validVehicleRC -> return (VALID, Just validVehicleRC)
            Nothing -> do
              let mVehicleRC = listToMaybe vehicleRCs
              case mVehicleRC of
                Just vehicleRC -> return (mapStatus vehicleRC.verificationStatus, Just vehicleRC)
                Nothing -> return (NO_DOC_AVAILABLE, Nothing)

mapStatus :: IV.VerificationStatus -> ResponseStatus
mapStatus = \case
  IV.PENDING -> PENDING
  IV.VALID -> VALID
  IV.INVALID -> INVALID

checkIfInVerification :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Image.ImageType -> Flow ResponseStatus
checkIfInVerification driverId merchantOpCityId onboardingTryLimit docType = do
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType
  images <- IQuery.findRecentByPersonIdAndImageType driverId merchantOpCityId docType
  pure $ verificationStatus onboardingTryLimit (length images) verificationReq

verificationStatus :: Int -> Int -> Maybe IV.IdfyVerification -> ResponseStatus
verificationStatus onboardingTryLimit imagesNum verificationReq =
  case verificationReq of
    Just req -> do
      if req.status == "pending"
        then PENDING
        else FAILED
    Nothing -> do
      if imagesNum > onboardingTryLimit
        then LIMIT_EXCEED
        else NO_DOC_AVAILABLE

enableDriver :: Id SP.Person -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ Nothing = return ()
enableDriver personId (Just dl) = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  unless driverInfo.enabled $ do
    DIQuery.verifyAndEnableDriver personId
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
