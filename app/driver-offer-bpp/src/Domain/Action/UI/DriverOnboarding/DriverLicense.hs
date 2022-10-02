{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.DriverLicense
  ( DriverDLReq (..),
    DriverDLRes,
    verifyDL,
    onVerifyDL,
  )
where

import qualified AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import qualified Data.Text as T
import Data.Time (nominalDay)
import qualified Data.Time as DT
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.Person as Person
import Environment
import qualified Idfy.Flow as Idfy
import qualified Idfy.Types as Idfy
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.Person as Person

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    imageId1 :: Id Image.Image,
    imageId2 :: Id Image.Image
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverDLRes = APISuccess

validateDriverDLReq :: UTCTime -> Validate DriverDLReq
validateDriverDLReq now DriverDLReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber licenseNum,
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t60YearsAgo t18YearsAgo
    ]
  where
    licenseNum = MinLength 5 `And` star (latinUC \/ digit)
    t18YearsAgo = yearsAgo 18
    t60YearsAgo = yearsAgo 60
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

verifyDL ::
  Id Person.Person ->
  DriverDLReq ->
  Flow DriverDLRes
verifyDL personId req@DriverDLReq {..} = do
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  image1 <- getImage imageId1
  image2 <- getImage imageId2

  resp <- Idfy.extractDLImage image1 (Just image2)
  case resp.result of
    Just result -> do
      unless ((removeSpaceAndDash <$> result.extraction_output.id_number) == (removeSpaceAndDash <$> Just driverLicenseNumber)) $
        throwImageError imageId1 ImageDocumentNumberMismatch
    Nothing -> throwImageError imageId1 ImageExtractionFailed

  eDl <- encrypt driverLicenseNumber
  mdriverLicense <- Query.findByDLNumber eDl

  case mdriverLicense of
    Just driverLicense -> do
      unless (driverLicense.driverId == personId) $ throwImageError imageId1 DLAlreadyLinked
      unless (driverLicense.licenseExpiry > now) $ throwImageError imageId1 DLAlreadyUpdated
      verifyDLFlow personId driverLicenseNumber driverDateOfBirth
    Nothing -> do
      mDriverDL <- Query.findByDriverId personId
      when (isJust mDriverDL) $ throwImageError imageId1 DriverAlreadyLinked
      verifyDLFlow personId driverLicenseNumber driverDateOfBirth
  return Success
  where
    getImage imageId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.imageType == Image.DriverLicense) $
        throwError (ImageInvalidType (show Image.DriverLicense) (show imageMetadata.imageType))
      S3.get $ T.unpack imageMetadata.s3Path

verifyDLFlow :: Id Person.Person -> Text -> UTCTime -> Flow ()
verifyDLFlow personId dlNumber driverDateOfBirth = do
  now <- getCurrentTime
  idfyRes <- Idfy.verifyDL dlNumber driverDateOfBirth
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
            docType = Image.DriverLicense,
            status = "pending",
            idfyResponse = Nothing,
            createdAt = now,
            updatedAt = now
          }

onVerifyDL :: Idfy.VerificationResponse -> Flow AckResponse
onVerifyDL [] = pure Ack
onVerifyDL [resp] = do
  verificationReq <- IVQuery.findByRequestId resp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  runTransaction $ IVQuery.updateResponse resp.request_id resp.status (show <$> resp.result)

  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  now <- getCurrentTime

  mDriverLicense <- mkDriverLicenseEntry person.id now resp.result
  case mDriverLicense of
    Just driverLicense -> do
      runTransaction $ Query.upsert driverLicense
      return Ack
    Nothing -> return Ack
onVerifyDL _ = pure Ack

mkDriverLicenseEntry :: Id Person.Person -> UTCTime -> Maybe Idfy.IdfyResult -> Flow (Maybe Domain.DriverLicense)
mkDriverLicenseEntry _ _ Nothing = return Nothing
mkDriverLicenseEntry driverId now (Just result) =
  case result.source_output of
    Just output -> do
      mEncryptedDL <- encrypt `mapM` output.id_number
      id <- generateGUID
      let mLicenseExpiry = convertTextToUTC output.nt_validity_to
      return $ createDL driverId output id now <$> mEncryptedDL <*> mLicenseExpiry
    Nothing -> return Nothing

createDL :: Id Person.Person -> Idfy.DLVerificationOutput -> Id Domain.DriverLicense -> UTCTime -> EncryptedHashedField 'AsEncrypted Text -> UTCTime -> Domain.DriverLicense
createDL driverId output id now edl expiry = do
  let classOfVehicles = map (.cov) output.cov_details
  let verificationStatus = validateDLStatus expiry classOfVehicles now
  Domain.DriverLicense
    { id,
      driverId,
      driverDob = convertTextToUTC output.dob,
      driverName = output.name,
      licenseNumber = edl,
      licenseExpiry = expiry,
      classOfVehicles,
      verificationStatus,
      failedRules = [],
      consent = True,
      createdAt = now,
      updatedAt = now,
      consentTimestamp = now
    }

validateDLStatus :: UTCTime -> [Idfy.ClassOfVehicle] -> UTCTime -> Domain.VerificationStatus
validateDLStatus expiry cov now = do
  let validCOV = foldr' (\x acc -> isValidCOVDL x || acc) False cov
  if (now < expiry) && validCOV then Domain.VALID else Domain.INVALID

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

dlValidCOV :: [Idfy.ClassOfVehicle]
dlValidCOV = [Idfy.LMV, Idfy.W_CAB, Idfy.LMV_NT, Idfy.LMV_T, Idfy.LMV_CAB, Idfy.LMV_HMV, Idfy.W_T]

isValidCOVDL :: Idfy.ClassOfVehicle -> Bool
isValidCOVDL cov = foldr' (\x acc -> x == cov || acc) False dlValidCOV

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""
