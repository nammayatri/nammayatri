module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusHandler,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import Domain.Types.DriverOnboarding.IdfyVerification
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.Person as SP
import Environment
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson

data ResponseStatus = VERIFICATION_PENDING | VERIFIED | VERIFICATION_FAILED | NO_DOC_AVAILABLE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: Id SP.Person -> Flow StatusRes
statusHandler personId = do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  dlStatus <- getDLStatus personId
  rcStatus <- getRCStatus personId

  when (dlStatus == VERIFIED && rcStatus == VERIFIED) $ DB.runTransaction $ Person.setRegisteredTrue personId
  return $ StatusRes {dlVerificationStatus = dlStatus, rcVerificationStatus = rcStatus}

getDLStatus :: Id SP.Person -> Flow ResponseStatus
getDLStatus driverId = do
  mDriverLicense <- DLQuery.findByDriverId driverId
  case mDriverLicense of
    Just driverLicense -> return $ mapStatus driverLicense.verificationStatus
    Nothing -> do
      checkIfInVerification driverId Image.DriverLicense

getRCStatus :: Id SP.Person -> Flow ResponseStatus
getRCStatus driverId = do
  mDriverAssociation <- DRAQuery.getActiveAssociationByDriver driverId
  case mDriverAssociation of
    Just driverAssociation -> do
      vehicleRC <- RCQuery.findById driverAssociation.rcId >>= fromMaybeM (InternalError "Associated rc not found")
      return $ mapStatus vehicleRC.verificationStatus
    Nothing -> do
      checkIfInVerification driverId Image.VehicleRegistrationCertificate

mapStatus :: VerificationStatus -> ResponseStatus
mapStatus = \case
  PENDING -> VERIFICATION_PENDING
  VALID -> VERIFIED
  INVALID -> VERIFICATION_FAILED

checkIfInVerification :: Id SP.Person -> Image.ImageType -> Flow ResponseStatus
checkIfInVerification driverId docType = do
  verificationReq <- IVQuery.findLatestByDriverIdAndDocType driverId docType
  return $ maybe NO_DOC_AVAILABLE (const VERIFICATION_PENDING) verificationReq
