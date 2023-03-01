{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.DriverLicense
  ( DriverDLReq (..),
    DriverDLRes,
    verifyDL,
    onVerifyDL,
    convertUTCTimetoDate,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time (nominalDay)
import qualified Data.Time as DT
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import qualified Idfy.Flow as Idfy
import qualified Idfy.Types as Idfy
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.OperatingCity as QCity
import qualified Storage.Queries.Person as Person
import Tools.Error

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    imageId1 :: Id Image.Image,
    imageId2 :: Maybe (Id Image.Image),
    dateOfIssue :: Maybe UTCTime
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
    t60YearsAgo = yearsAgo 80
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

verifyDL ::
  Bool ->
  Maybe DM.Merchant ->
  Id Person.Person ->
  DriverDLReq ->
  Flow DriverDLRes
verifyDL isDashboard mbMerchant personId req@DriverDLReq {..} = do
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  person <- Person.findById (Proxy @Flow) personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  operatingCity' <- case mbMerchant of
    Just merchant -> QCity.findEnabledCityByMerchantIdAndName merchant.id (T.toLower req.operatingCity) (Proxy @Flow)
    Nothing -> QCity.findEnabledCityByName (T.toLower req.operatingCity) (Proxy @Flow)
  when (null operatingCity') $
    throwError $ InvalidOperatingCity req.operatingCity
  configs <- asks (.driverOnboardingConfigs)

  when
    ( isNothing dateOfIssue && configs.checkImageExtraction
        && (not isDashboard || configs.checkImageExtractionForDashboard)
    )
    $ do
      image1 <- getImage imageId1
      image2 <- getImage `mapM` imageId2
      resp <- Idfy.extractDLImage image1 image2
      case resp.result of
        Just result -> do
          let extractDLNumber = removeSpaceAndDash <$> result.extraction_output.id_number
          let dlNumber = removeSpaceAndDash <$> Just driverLicenseNumber
          -- disable this check for debugging with mock-idfy
          unless (extractDLNumber == dlNumber) $
            throwImageError imageId1 $ ImageDocumentNumberMismatch (maybe "null" maskText extractDLNumber) (maybe "null" maskText dlNumber)
        Nothing -> throwImageError imageId1 ImageExtractionFailed
  mdriverLicense <- Query.findByDLNumber driverLicenseNumber (Proxy @Flow)

  case mdriverLicense of
    Just driverLicense -> do
      unless (driverLicense.driverId == personId) $ throwImageError imageId1 DLAlreadyLinked
      unless (driverLicense.licenseExpiry > now) $ throwImageError imageId1 DLAlreadyUpdated
      verifyDLFlow personId driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue
    Nothing -> do
      mDriverDL <- Query.findByDriverId personId (Proxy @Flow)
      when (isJust mDriverDL) $ throwImageError imageId1 DriverAlreadyLinked
      verifyDLFlow personId driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId = do
      imageMetadata <- ImageQuery.findById (Proxy @Flow) imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
      unless (imageMetadata.imageType == Image.DriverLicense) $
        throwError (ImageInvalidType (show Image.DriverLicense) (show imageMetadata.imageType))
      S3.get $ T.unpack imageMetadata.s3Path

verifyDLFlow :: Id Person.Person -> Text -> UTCTime -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe UTCTime -> Flow ()
verifyDLFlow personId dlNumber driverDateOfBirth imageId1 imageId2 dateOfIssue = do
  now <- getCurrentTime
  configs <- asks (.driverOnboardingConfigs)
  let imageExtractionValidation =
        if isNothing dateOfIssue && configs.checkImageExtraction
          then Domain.Success
          else Domain.Skipped
  idfyRes <- Idfy.verifyDL dlNumber driverDateOfBirth
  encryptedDL <- encrypt dlNumber
  idfyVerificationEntity <- mkIdfyVerificationEntity idfyRes.request_id now imageExtractionValidation encryptedDL
  runTransaction $ IVQuery.create @Flow idfyVerificationEntity
  where
    mkIdfyVerificationEntity requestId now imageExtractionValidation encryptedDL = do
      id <- generateGUID
      return $
        Domain.IdfyVerification
          { id,
            driverId = personId,
            documentImageId1 = imageId1,
            documentImageId2 = imageId2,
            requestId,
            imageExtractionValidation = imageExtractionValidation,
            documentNumber = encryptedDL,
            issueDateOnDoc = dateOfIssue,
            docType = Image.DriverLicense,
            status = "pending",
            idfyResponse = Nothing,
            createdAt = now,
            updatedAt = now
          }

onVerifyDL :: Domain.IdfyVerification -> Idfy.DLVerificationOutput -> Flow AckResponse
onVerifyDL verificationReq output = do
  person <- Person.findById (Proxy @Flow) verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)

  if verificationReq.imageExtractionValidation == Domain.Skipped
    && isJust verificationReq.issueDateOnDoc
    && ( (convertUTCTimetoDate <$> verificationReq.issueDateOnDoc)
           /= (convertUTCTimetoDate <$> (convertTextToUTC output.date_of_issue))
       )
    then runTransaction $ IVQuery.updateExtractValidationStatus @Flow verificationReq.requestId Domain.Failed >> pure Ack
    else do
      now <- getCurrentTime
      id <- generateGUID
      configs <- asks (.driverOnboardingConfigs)

      mEncryptedDL <- encrypt `mapM` output.id_number
      let mLicenseExpiry = convertTextToUTC (output.t_validity_to <|> output.nt_validity_to)
      let mDriverLicense = createDL configs person.id output id verificationReq.documentImageId1 verificationReq.documentImageId2 now <$> mEncryptedDL <*> mLicenseExpiry

      case mDriverLicense of
        Just driverLicense -> do
          runTransaction $ Query.upsert @Flow driverLicense
          case driverLicense.driverName of
            Just name_ -> runTransaction $ Person.updateName @Flow person.id name_
            Nothing -> return ()
          return Ack
        Nothing -> return Ack

createDL ::
  DriverOnboardingConfigs ->
  Id Person.Person ->
  Idfy.DLVerificationOutput ->
  Id Domain.DriverLicense ->
  Id Image.Image ->
  Maybe (Id Image.Image) ->
  UTCTime ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  Domain.DriverLicense
createDL configs driverId output id imageId1 imageId2 now edl expiry = do
  let classOfVehicles = maybe [] (map (.cov)) output.cov_details
  let verificationStatus = validateDLStatus configs expiry classOfVehicles now
  Domain.DriverLicense
    { id,
      driverId,
      documentImageId1 = imageId1,
      documentImageId2 = imageId2,
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

validateDLStatus :: DriverOnboardingConfigs -> UTCTime -> [Text] -> UTCTime -> Domain.VerificationStatus
validateDLStatus configs expiry cov now = do
  let validCOVs = configs.validDLVehicleClassInfixes
  let isCOVValid = (not configs.checkDLVehicleClass) || foldr' (\x acc -> isValidCOVDL validCOVs x || acc) False cov
  if ((not configs.checkDLExpiry) || now < expiry) && isCOVValid then Domain.VALID else Domain.INVALID

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVDL :: [Text] -> Text -> Bool
isValidCOVDL validCOVs cov = foldr' (\x acc -> T.isInfixOf (T.toUpper x) (T.toUpper cov) || acc) False validCOVs

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

convertUTCTimetoDate :: UTCTime -> Text
convertUTCTimetoDate utctime = T.pack (DT.formatTime DT.defaultTimeLocale "%d/%m/%Y" utctime)
