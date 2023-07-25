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

import Domain.Types.DriverOnboarding.AadhaarVerification as AV
import qualified Domain.Types.DriverOnboarding.DriverLicense as DL
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Vehicle
import Domain.Types.Vehicle.Variant
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Error
import qualified Storage.CachedQueries.DriverInformation as DIQuery
import Storage.CachedQueries.Merchant.TransporterConfig
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as SAV
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as IQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as VQuery

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

statusHandler :: (Id SP.Person, Id DM.Merchant) -> Flow StatusRes
statusHandler (personId, merchantId) = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  (dlStatus, mDL) <- getDLAndStatus personId transporterConfig.onboardingTryLimit
  (rcStatus, mRC) <- getRCAndStatus personId transporterConfig.onboardingTryLimit
  (aadhaarStatus, _) <- getAadhaarStatus personId transporterConfig.aadhaarVerificationRequired
  when (rcStatus == VALID) $ do
    createVehicle personId merchantId mRC
  when (dlStatus == VALID && rcStatus == VALID && aadhaarStatus == VALID) $ do
    enableDriver personId merchantId mDL
  return $ StatusRes {dlVerificationStatus = dlStatus, rcVerificationStatus = rcStatus, aadhaarVerificationStatus = aadhaarStatus}

getAadhaarStatus :: Id SP.Person -> Bool -> Flow (ResponseStatus, Maybe AV.AadhaarVerification)
getAadhaarStatus _ False = return (VALID, Nothing)
getAadhaarStatus personId True = do
  mAadhaarCard <- SAV.findByDriverId personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.isVerified
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id SP.Person -> Int -> Flow (ResponseStatus, Maybe DL.DriverLicense)
getDLAndStatus driverId onboardingTryLimit = do
  mDriverLicense <- DLQuery.findByDriverId driverId
  status <-
    case mDriverLicense of
      Just driverLicense -> return $ mapStatus driverLicense.verificationStatus
      Nothing -> do
        checkIfInVerification driverId onboardingTryLimit Image.DriverLicense
  return (status, mDriverLicense)

getRCAndStatus :: Id SP.Person -> Int -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate)
getRCAndStatus driverId onboardingTryLimit = do
  mDriverAssociation <- DRAQuery.getActiveAssociationByDriver driverId
  case mDriverAssociation of
    Just driverAssociation -> do
      vehicleRC <- RCQuery.findById driverAssociation.rcId >>= fromMaybeM (InternalError "Associated rc not found")
      return (mapStatus vehicleRC.verificationStatus, Just vehicleRC)
    Nothing -> do
      status <- checkIfInVerification driverId onboardingTryLimit Image.VehicleRegistrationCertificate
      return (status, Nothing)

mapStatus :: IV.VerificationStatus -> ResponseStatus
mapStatus = \case
  IV.PENDING -> PENDING
  IV.VALID -> VALID
  IV.INVALID -> INVALID

checkIfInVerification :: Id SP.Person -> Int -> Image.ImageType -> Flow ResponseStatus
checkIfInVerification driverId onboardingTryLimit docType = do
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
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

enableDriver :: Id SP.Person -> Id DM.Merchant -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing = return ()
enableDriver personId _ (Just dl) = do
  DIQuery.verifyAndEnableDriver personId
  case dl.driverName of
    Just name -> DB.runTransaction $ Person.updateName personId name
    Nothing -> return ()

createVehicle :: Id SP.Person -> Id DM.Merchant -> Maybe RC.VehicleRegistrationCertificate -> Flow ()
createVehicle _ _ Nothing = return ()
createVehicle personId merchantId (Just rc) = do
  rcNumber <- decrypt rc.certificateNumber
  now <- getCurrentTime
  let vehicle = buildVehicle now personId merchantId rcNumber
  _ <- VQuery.upsert vehicle
  case dl.driverName of
    Just name -> void (Person.updateName personId name)
    Nothing -> return ()
  where
    buildVehicle now personId_ merchantId_ certificateNumber =
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
