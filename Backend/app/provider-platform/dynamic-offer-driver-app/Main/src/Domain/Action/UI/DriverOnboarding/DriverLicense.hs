{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

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
import Domain.Types.Merchant.OnboardingDocumentConfig (OnboardingDocumentConfig)
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as DTO
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Merchant.MerchantConfig as QTC
import qualified Storage.CachedQueries.Merchant.OnboardingDocumentConfig as QODC
import qualified Storage.Queries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.OperatingCity as QCity
import qualified Storage.Queries.Person as Person
import Tools.Error
import qualified Tools.Verification as Verification

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
  (Id Person.Person, Id DM.Merchant) ->
  DriverDLReq ->
  Flow DriverDLRes
verifyDL isDashboard mbMerchant (personId, _) req@DriverDLReq {..} = do
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  operatingCity' <- case mbMerchant of
    Just merchant -> QCity.findEnabledCityByMerchantIdAndName merchant.id $ T.toLower req.operatingCity
    Nothing -> QCity.findEnabledCityByName $ T.toLower req.operatingCity
  when (null operatingCity') $
    throwError $ InvalidOperatingCity $ T.toLower req.operatingCity
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  onboardingDocumentConfig <- QODC.findByMerchantIdAndDocumentType person.merchantId DTO.DL >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show DTO.DL))
  when
    ( isNothing dateOfIssue && onboardingDocumentConfig.checkExtraction
        && (not isDashboard || transporterConfig.checkImageExtractionForDashboard)
    )
    $ do
      image1 <- getImage imageId1
      image2 <- getImage `mapM` imageId2
      resp <-
        Verification.extractDLImage person.merchantId $
          Verification.ExtractImageReq {image1, image2, driverId = person.id.getId}
      case resp.extractedDL of
        Just extractedDL -> do
          let extractDLNumber = removeSpaceAndDash <$> extractedDL.dlNumber
          let dlNumber = removeSpaceAndDash <$> Just driverLicenseNumber
          -- disable this check for debugging with mock-idfy
          cacheExtractedDl person.id extractDLNumber operatingCity
          unless (extractDLNumber == dlNumber) $
            throwImageError imageId1 $ ImageDocumentNumberMismatch (maybe "null" maskText extractDLNumber) (maybe "null" maskText dlNumber)
        Nothing -> throwImageError imageId1 ImageExtractionFailed
  mdriverLicense <- Query.findByDLNumber driverLicenseNumber

  case mdriverLicense of
    Just driverLicense -> do
      unless (driverLicense.driverId == personId) $ throwImageError imageId1 DLAlreadyLinked
      unless (driverLicense.licenseExpiry > now) $ throwImageError imageId1 DLAlreadyUpdated
      verifyDLFlow person onboardingDocumentConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue
    Nothing -> do
      mDriverDL <- Query.findByDriverId personId
      when (isJust mDriverDL) $ throwImageError imageId1 DriverAlreadyLinked
      verifyDLFlow person onboardingDocumentConfig driverLicenseNumber driverDateOfBirth imageId1 imageId2 dateOfIssue
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
      unless (imageMetadata.imageType == Image.DriverLicense) $
        throwError (ImageInvalidType (show Image.DriverLicense) (show imageMetadata.imageType))
      S3.get $ T.unpack imageMetadata.s3Path

verifyDLFlow :: Person.Person -> OnboardingDocumentConfig -> Text -> UTCTime -> Id Image.Image -> Maybe (Id Image.Image) -> Maybe UTCTime -> Flow ()
verifyDLFlow person onboardingDocumentConfig dlNumber driverDateOfBirth imageId1 imageId2 dateOfIssue = do
  now <- getCurrentTime
  let imageExtractionValidation =
        if isNothing dateOfIssue && onboardingDocumentConfig.checkExtraction
          then Domain.Success
          else Domain.Skipped
  verifyRes <-
    Verification.verifyDLAsync person.merchantId $
      Verification.VerifyDLAsyncReq {dlNumber, dateOfBirth = driverDateOfBirth, driverId = person.id.getId}
  encryptedDL <- encrypt dlNumber
  idfyVerificationEntity <- mkIdfyVerificationEntity verifyRes.requestId now imageExtractionValidation encryptedDL
  IVQuery.create idfyVerificationEntity
  where
    mkIdfyVerificationEntity requestId now imageExtractionValidation encryptedDL = do
      id <- generateGUID
      return $
        Domain.IdfyVerification
          { id,
            driverId = person.id,
            documentImageId1 = imageId1,
            documentImageId2 = imageId2,
            requestId,
            imageExtractionValidation = imageExtractionValidation,
            documentNumber = encryptedDL,
            issueDateOnDoc = dateOfIssue,
            driverDateOfBirth = Just driverDateOfBirth,
            docType = Image.DriverLicense,
            status = "pending",
            idfyResponse = Nothing,
            multipleRC = Nothing, -- added for backward compatibility
            dashboardPassedVehicleVariant = Nothing,
            createdAt = now,
            updatedAt = now
          }

onVerifyDL :: Domain.IdfyVerification -> Idfy.DLVerificationOutput -> Flow AckResponse
onVerifyDL verificationReq output = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  let key = dlCacheKey person.id
  extractedDlAndOperatingCity <- Redis.safeGet key
  void $ Redis.del key
  case (output.status, verificationReq.issueDateOnDoc, extractedDlAndOperatingCity, verificationReq.driverDateOfBirth) of
    (Just status, Just issueDate, Just (extractedDL, operatingCity), Just dob) | status == "id_not_found" -> dlNotFoundFallback issueDate (extractedDL, operatingCity) dob verificationReq person
    _ -> linkDl person
  where
    linkDl :: Person.Person -> Flow AckResponse
    linkDl person = do
      if verificationReq.imageExtractionValidation == Domain.Skipped
        && isJust verificationReq.issueDateOnDoc
        && ( (convertUTCTimetoDate <$> verificationReq.issueDateOnDoc)
               /= (convertUTCTimetoDate <$> (convertTextToUTC output.date_of_issue))
           )
        then do
          _ <- IVQuery.updateExtractValidationStatus verificationReq.requestId Domain.Failed
          pure Ack
        else do
          now <- getCurrentTime
          id <- generateGUID
          onboardingDocumentConfig <- QODC.findByMerchantIdAndDocumentType person.merchantId DTO.DL >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantId.getId (show DTO.DL))
          mEncryptedDL <- encrypt `mapM` output.id_number
          let mLicenseExpiry = convertTextToUTC (output.t_validity_to <|> output.nt_validity_to)
          let mDriverLicense = createDL onboardingDocumentConfig person.id output id verificationReq.documentImageId1 verificationReq.documentImageId2 now <$> mEncryptedDL <*> mLicenseExpiry

          case mDriverLicense of
            Just driverLicense -> do
              Query.upsert driverLicense
              case driverLicense.driverName of
                Just name_ -> void $ Person.updateName person.id name_
                Nothing -> pure ()
              return Ack
            Nothing -> return Ack

dlCacheKey :: Id Person.Person -> Text
dlCacheKey personId =
  "providerPlatform:dlCacheKey:" <> personId.getId

createDL ::
  DTO.OnboardingDocumentConfig ->
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

validateDLStatus :: DTO.OnboardingDocumentConfig -> UTCTime -> [Text] -> UTCTime -> Domain.VerificationStatus
validateDLStatus configs expiry cov now = do
  case configs.supportedVehicleClasses of
    DTO.DLValidClasses [] -> Domain.INVALID
    DTO.DLValidClasses validCOVs -> do
      let validCOVsCheck = configs.vehicleClassCheckType
      let isCOVValid = foldr' (\x acc -> isValidCOVDL validCOVs validCOVsCheck x || acc) False cov
      if ((not configs.checkExpiry) || now < expiry) && isCOVValid then Domain.VALID else Domain.INVALID
    _ -> Domain.INVALID

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

isValidCOVDL :: [Text] -> DTO.VehicleClassCheckType -> Text -> Bool
isValidCOVDL validCOVs validCOVsCheck cov =
  checkForClass
  where
    checkForClass = foldr' (\x acc -> classCheckFunction validCOVsCheck (T.toUpper x) (T.toUpper cov) || acc) False validCOVs

classCheckFunction :: DTO.VehicleClassCheckType -> Text -> Text -> Bool
classCheckFunction validCOVsCheck =
  case validCOVsCheck of
    DTO.Infix -> T.isInfixOf
    DTO.Prefix -> T.isPrefixOf
    DTO.Suffix -> T.isSuffixOf

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

convertUTCTimetoDate :: UTCTime -> Text
convertUTCTimetoDate utctime = T.pack (DT.formatTime DT.defaultTimeLocale "%d/%m/%Y" utctime)

cacheExtractedDl :: Id Person.Person -> Maybe Text -> Text -> Flow ()
cacheExtractedDl _ Nothing _ = return ()
cacheExtractedDl personId extractedDL operatingCity = do
  let key = dlCacheKey personId
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  Redis.setExp key (extractedDL, operatingCity) authTokenCacheExpiry

dlNotFoundFallback :: UTCTime -> (Text, Text) -> UTCTime -> Domain.IdfyVerification -> Person.Person -> Flow AckResponse
dlNotFoundFallback issueDate (extractedDL, operatingCity) dob verificationReq person = do
  let dlreq =
        DriverDLReq
          { driverLicenseNumber = extractedDL,
            operatingCity = operatingCity,
            driverDateOfBirth = dob,
            imageId1 = verificationReq.documentImageId1,
            imageId2 = verificationReq.documentImageId2,
            dateOfIssue = Just issueDate
          }
  void $ verifyDL False Nothing (person.id, person.merchantId) dlreq
  return Ack
