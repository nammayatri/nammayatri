{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
  ( DriverRCReq (..),
    DriverRCRes,
    verifyRC,
    onVerifyRC,
  )
where

import AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.DriverOnboarding.DriverRCAssociation as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Person as Person
import Environment
import qualified Idfy.Flow as Idfy
import qualified Idfy.Types as Idfy
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.Person as Person

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

verifyRC ::
  Id Person.Person ->
  DriverRCReq ->
  Flow DriverRCRes
verifyRC personId req@DriverRCReq {..} = do
  runRequestValidation validateDriverRCReq req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
  unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
  unless (imageMetadata.imageType == Image.VehicleRegistrationCertificate) $
    throwError (ImageInvalidType (show Image.VehicleRegistrationCertificate) (show imageMetadata.imageType))

  image <- S3.get (T.unpack imageMetadata.s3Path)
  resp <- Idfy.extractRCImage image Nothing
  case resp.result of
    Just result -> do
      let extractRCNumber = removeSpaceAndDash <$> result.extraction_output.registration_number
      let rcNumber = removeSpaceAndDash <$> Just vehicleRegistrationCertNumber
      unless (extractRCNumber == rcNumber) $
        throwImageError imageId $ ImageDocumentNumberMismatch (maybe "null" maskText extractRCNumber) (maybe "null" maskText rcNumber)
    Nothing -> throwImageError imageId ImageExtractionFailed

  now <- getCurrentTime
  mDriverAssociation <- DAQuery.getActiveAssociationByDriver personId

  case mDriverAssociation of
    Just driverAssociaion -> do
      driverRC <- RCQuery.findById driverAssociaion.rcId >>= fromMaybeM (InvalidRequest "Missing RC entry")
      rcNumber <- decrypt driverRC.certificateNumber
      unless (rcNumber == vehicleRegistrationCertNumber) $ throwImageError imageId DriverAlreadyLinked
      unless (driverRC.fitnessExpiry < now) $ throwImageError imageId RCAlreadyUpdated -- RC not expired
      verifyRCFlow personId vehicleRegistrationCertNumber
    Nothing -> do
      eRC <- encrypt vehicleRegistrationCertNumber
      mVehicleRC <- RCQuery.findLastVehicleRC eRC
      case mVehicleRC of
        Just vehicleRC -> do
          mRCAssociation <- DAQuery.getActiveAssociationByRC vehicleRC.id
          when (isJust mRCAssociation) $ throwImageError imageId RCAlreadyLinked
          verifyRCFlow personId vehicleRegistrationCertNumber
        Nothing -> do
          verifyRCFlow personId vehicleRegistrationCertNumber

  return Success

verifyRCFlow :: Id Person.Person -> Text -> Flow ()
verifyRCFlow personId rcNumber = do
  now <- getCurrentTime
  idfyRes <- Idfy.verifyRC rcNumber
  idfyVerificationEntity <- mkIdfyVerificationEntity idfyRes.request_id now
  runTransaction $ IVQuery.create idfyVerificationEntity
  where
    mkIdfyVerificationEntity requestId now = do
      id <- generateGUID
      return $
        Domain.IdfyVerification
          { id,
            driverId = personId,
            requestId,
            docType = Image.VehicleRegistrationCertificate,
            status = "PENDING",
            idfyResponse = Nothing,
            createdAt = now,
            updatedAt = now
          }

onVerifyRC :: Idfy.VerificationResponse -> Flow AckResponse
onVerifyRC [] = pure Ack
onVerifyRC [resp] = do
  verificationReq <- IVQuery.findByRequestId resp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  runTransaction $ IVQuery.updateResponse resp.request_id resp.status (show <$> resp.result)

  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  now <- getCurrentTime

  mVehicleRC <- mkVehicleRCEntry now resp.result
  case mVehicleRC of
    Just vehicleRC -> do
      runTransaction $ RCQuery.upsert vehicleRC

      -- linking to driver
      rc <- RCQuery.findByRCAndExpiry vehicleRC.certificateNumber vehicleRC.fitnessExpiry >>= fromMaybeM (InternalError "RC not found")
      mRCAssociation <- DAQuery.getActiveAssociationByRC rc.id
      when (isNothing mRCAssociation) $ do
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
            associatedTill = Nothing,
            consent = True,
            consentTimestamp = now
          }
onVerifyRC _ = pure Ack

mkVehicleRCEntry :: UTCTime -> Maybe Idfy.IdfyResult -> Flow (Maybe Domain.VehicleRegistrationCertificate)
mkVehicleRCEntry _ Nothing = return Nothing
mkVehicleRCEntry now (Just result) =
  case result.extraction_output of
    Just output -> do
      mEncryptedRC <- encrypt `mapM` output.registration_number
      id <- generateGUID
      let mbFitnessEpiry = convertTextToUTC output.fitness_upto
      return $ createRC output id now <$> mEncryptedRC <*> mbFitnessEpiry
    Nothing -> return Nothing

createRC :: Idfy.RCVerificationOutput -> Id Domain.VehicleRegistrationCertificate -> UTCTime -> EncryptedHashedField 'AsEncrypted Text -> UTCTime -> Domain.VehicleRegistrationCertificate
createRC output id now edl expiry = do
  let insuranceValidity = convertTextToUTC output.insurance_validity
  let vehicleClass = output.vehicle_class
  let verificationStatus = validateRCStatus expiry insuranceValidity vehicleClass now
  Domain.VehicleRegistrationCertificate
    { id,
      certificateNumber = edl,
      fitnessExpiry = expiry,
      permitExpiry = convertTextToUTC output.permit_validity,
      pucExpiry = convertTextToUTC output.puc_number_upto,
      vehicleClass,
      vehicleManufacturer = output.manufacturer,
      insuranceValidity,
      verificationStatus,
      failedRules = [],
      createdAt = now,
      updatedAt = now
    }

validateRCStatus :: UTCTime -> Maybe UTCTime -> Maybe Text -> UTCTime -> Domain.VerificationStatus
validateRCStatus expiry insuranceValidity cov now = do
  let validCOV = maybe False isValidCOVRC cov
  let validInsurance = maybe False (now <) insuranceValidity
  if (now < expiry) && validCOV && validInsurance then Domain.VALID else Domain.INVALID

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVRC :: Text -> Bool
isValidCOVRC = T.isInfixOf "3WT"

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""
