module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusHandler,
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import Data.Text as T hiding (length)
import qualified Domain.Types.DriverOnboarding.DriverLicense as DL
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.Organization as Org
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

data ResponseStatus = PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | NO_DOC_AVAILABLE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: Id SP.Person -> Flow StatusRes
statusHandler personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")

  (dlStatus, mDL) <- getDLAndStatus personId
  (rcStatus, mRC) <- getRCAndStatus personId

  when (dlStatus == VALID && rcStatus == VALID) $
    enableDriver personId orgId mRC mDL
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
  case verificationReq of
    Just req -> do
      if req.status == T.pack "failed"
        then return FAILED
        else return PENDING
    Nothing -> do
      images <- IQuery.findRecentByPersonIdAndImageType driverId docType
      onboardingTryLimit <- asks (.onboardingTryLimit)
      if length images > onboardingTryLimit
        then return LIMIT_EXCEED
        else return NO_DOC_AVAILABLE

enableDriver :: Id SP.Person -> Id Org.Organization -> Maybe RC.VehicleRegistrationCertificate -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing Nothing = return ()
enableDriver personId orgId (Just rc) (Just dl) = do
  DB.runTransaction $ DIQuery.verifyAndEnableDriver personId
  rcNumber <- decrypt rc.certificateNumber
  mVehicle <- VQuery.findByRegistrationNo rcNumber
  when (isNothing mVehicle) $ do
    now <- getCurrentTime
    let vehicle = buildVehicle now personId orgId rcNumber
    DB.runTransaction $ VQuery.create vehicle
    case dl.driverName of
      Just name -> DB.runTransaction $ Person.updateName personId name
      Nothing -> return ()
  where
    buildVehicle now personId_ orgId_ certificateNumber =
      Vehicle.Vehicle
        { Vehicle.driverId = personId_,
          Vehicle.capacity = Nothing,
          Vehicle.category = Just Vehicle.AUTO_CATEGORY,
          Vehicle.make = Nothing,
          Vehicle.model = "2010",
          Vehicle.size = Nothing,
          Vehicle.organizationId = orgId_,
          Vehicle.variant = AUTO_RICKSHAW,
          Vehicle.color = "Yellow",
          Vehicle.energyType = Nothing,
          Vehicle.registrationNo = certificateNumber,
          Vehicle.registrationCategory = Nothing,
          Vehicle.createdAt = now,
          Vehicle.updatedAt = now
        }
enableDriver _ _ _ _ = return ()
