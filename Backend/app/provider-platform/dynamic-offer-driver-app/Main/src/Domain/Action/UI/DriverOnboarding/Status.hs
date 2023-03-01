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
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Error
import qualified Storage.CachedQueries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as IQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as VQuery

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- INVALID is the state
--   which the doc switches to when, for example, it's expired.
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: Id SP.Person -> Flow StatusRes
statusHandler personId = do
  person <- runInReplica $ QPerson.findById (Proxy @Flow) personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  (dlStatus, mDL) <- getDLAndStatus personId
  (rcStatus, mRC) <- getRCAndStatus personId

  when (dlStatus == VALID && rcStatus == VALID) $
    enableDriver personId person.merchantId mRC mDL
  return $ StatusRes {dlVerificationStatus = dlStatus, rcVerificationStatus = rcStatus}

getDLAndStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe DL.DriverLicense)
getDLAndStatus driverId = do
  mDriverLicense <- DLQuery.findByDriverId driverId (Proxy @Flow)
  status <-
    case mDriverLicense of
      Just driverLicense -> return $ mapStatus driverLicense.verificationStatus
      Nothing -> do
        checkIfInVerification driverId Image.DriverLicense
  return (status, mDriverLicense)

getRCAndStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate)
getRCAndStatus driverId = do
  mDriverAssociation <- DRAQuery.getActiveAssociationByDriver driverId (Proxy @Flow)
  case mDriverAssociation of
    Just driverAssociation -> do
      vehicleRC <- RCQuery.findById (Proxy @Flow) driverAssociation.rcId >>= fromMaybeM (InternalError "Associated rc not found")
      return (mapStatus vehicleRC.verificationStatus, Just vehicleRC)
    Nothing -> do
      status <- checkIfInVerification driverId Image.VehicleRegistrationCertificate
      return (status, Nothing)

mapStatus :: IV.VerificationStatus -> ResponseStatus
mapStatus = \case
  IV.PENDING -> PENDING
  IV.VALID -> VALID
  IV.INVALID -> INVALID

checkIfInVerification :: Id SP.Person -> Image.ImageType -> Flow ResponseStatus
checkIfInVerification driverId docType = do
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType (Proxy @Flow)
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType (Proxy @Flow)
  onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
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

enableDriver :: Id SP.Person -> Id DM.Merchant -> Maybe RC.VehicleRegistrationCertificate -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing Nothing = return ()
enableDriver personId merchantId (Just rc) (Just dl) = do
  DIQuery.verifyAndEnableDriver personId
  rcNumber <- decrypt rc.certificateNumber
  now <- getCurrentTime
  let vehicle = buildVehicle now personId merchantId rcNumber
  DB.runTransaction $ VQuery.upsert @Flow vehicle
  case dl.driverName of
    Just name -> DB.runTransaction $ Person.updateName @Flow personId name
    Nothing -> return ()
  where
    buildVehicle now personId_ merchantId_ certificateNumber =
      Vehicle.Vehicle
        { Vehicle.driverId = personId_,
          Vehicle.capacity = rc.vehicleCapacity,
          Vehicle.category = Just Vehicle.AUTO_CATEGORY,
          Vehicle.make = rc.vehicleManufacturer,
          Vehicle.model = fromMaybe "" rc.vehicleModel,
          Vehicle.size = Nothing,
          Vehicle.merchantId = merchantId_,
          Vehicle.variant = AUTO_RICKSHAW,
          Vehicle.color = fromMaybe "Yellow" rc.vehicleColor,
          Vehicle.energyType = rc.vehicleEnergyType,
          Vehicle.registrationNo = certificateNumber,
          Vehicle.registrationCategory = Nothing,
          Vehicle.vehicleClass = "3WT",
          Vehicle.createdAt = now,
          Vehicle.updatedAt = now
        }
enableDriver _ _ _ _ = return ()
