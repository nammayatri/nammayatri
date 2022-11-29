module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusHandler,
    mapStatus,
    verificationStatus,
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Storage.Esqueleto.Transactionable (runInReplica)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import qualified Domain.Types.DriverOnboarding.DriverLicense as DL
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Vehicle
import Domain.Types.Vehicle.Variant
import Environment
import qualified Storage.Queries.DriverInformation as DIQuery
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
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")

  (dlStatus, mDL) <- getDLAndStatus personId
  (rcStatus, mRC) <- getRCAndStatus personId

  when (dlStatus == VALID && rcStatus == VALID) $
    enableDriver personId merchantId mRC mDL
  return $ StatusRes {dlVerificationStatus = dlStatus, rcVerificationStatus = rcStatus}

getDLAndStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe DL.DriverLicense)
getDLAndStatus driverId = do
  mDriverLicense <- DLQuery.findByDriverId driverId
  status <-
    case mDriverLicense of
      Just driverLicense -> return $ mapStatus driverLicense.verificationStatus
      Nothing -> do
        checkIfInVerification driverId Image.DriverLicense
  return (status, mDriverLicense)

getRCAndStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate)
getRCAndStatus driverId = do
  mDriverAssociation <- DRAQuery.getActiveAssociationByDriver driverId
  case mDriverAssociation of
    Just driverAssociation -> do
      vehicleRC <- RCQuery.findById driverAssociation.rcId >>= fromMaybeM (InternalError "Associated rc not found")
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
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
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
  DB.runTransaction $ DIQuery.verifyAndEnableDriver personId
  rcNumber <- decrypt rc.certificateNumber
  now <- getCurrentTime
  let vehicle = buildVehicle now personId merchantId rcNumber
  DB.runTransaction $ VQuery.upsert vehicle
  case dl.driverName of
    Just name -> DB.runTransaction $ Person.updateName personId name
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
