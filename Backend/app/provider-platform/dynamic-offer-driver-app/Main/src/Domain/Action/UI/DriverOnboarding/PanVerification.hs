{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.PanVerification
  ( DriverPanReq (..),
    DriverPanRes,
    verifyPan,
    onVerifyPan,
  )
where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as DO
import Control.Applicative (liftA2)
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import qualified Control.Monad.Extra as CME
import Data.Aeson hiding (Success)
import Data.Text as T hiding (elem, find, length, map, zip)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Tuple.Extra (both)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DFR
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import Domain.Types.DocumentVerificationConfig (DocumentVerificationConfig)
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.VehicleCategory
import Environment
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (find, null)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as QODC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Tools.Error
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

data DriverPanReq = DriverPanReq
  { panNumber :: Text,
    imageId :: Text, --Image,
    driverId :: Text,
    panName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverPanRes = APISuccess

verifyPan ::
  DPan.VerifiedBy ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverPanReq ->
  Maybe Bool ->
  Bool ->
  Flow Bool
verifyPan verifyBy mbMerchant (personId, _, merchantOpCityId) req adminApprovalRequired isDashboard = do
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyPanHitsCountKey req.panNumber) externalServiceRateLimitOptions
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  (blocked, driverDocument) <- DVRC.getDriverDocumentInfo person
  now <- getCurrentTime
  when blocked $ throwError AccountBlocked
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  case transporterConfig.allowDuplicatePan of
    Just False -> do
      panHash <- getDbHash req.panNumber
      panInfoList <- DPQuery.findAllByEncryptedPanNumber panHash
      when (length panInfoList > 1) $ throwError PanAlreadyLinked
      panPersonDetails <- Person.getDriversByIdIn (map (.driverId) panInfoList)
      let getRoles = map (.role) panPersonDetails
      when (person.role `elem` getRoles) $ throwError PanAlreadyLinked
    _ -> pure ()

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbPanVerificationService =
        (if isDashboard then merchantServiceUsageConfig.dashboardPanVerificationService else merchantServiceUsageConfig.panVerificationService)
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  let runBody = do
        mdriverPanInformation <- DPQuery.findByDriverId person.id
        case mbPanVerificationService of
          Just VI.HyperVerge -> do
            let panReq = DO.DriverPanReq {panNumber = req.panNumber, imageId1 = Id req.imageId, imageId2 = Nothing, consent = True, nameOnCard = Nothing, dateOfBirth = Nothing, consentTimestamp = Nothing, validationStatus = Nothing, verifiedBy = Nothing, transactionId = Nothing, nameOnGovtDB = Nothing, docType = Nothing}
            void $ checkIfGenuineReq panReq person
            panCardDetails <- buildPanCard person Nothing Nothing Nothing (Just verifyBy) (Id req.imageId) req.panNumber
            DPQuery.create panCardDetails
          Just VI.Idfy -> do
            void $ callIdfy person mdriverPanInformation driverDocument transporterConfig
          _ -> do
            panCardDetails <- buildPanCard person Nothing Nothing Nothing (Just verifyBy) (Id req.imageId) req.panNumber
            DPQuery.create $ panCardDetails
        case person.role of
          role | DCommon.checkFleetOwnerRole role -> do
            encryptedPanNumber <- encrypt req.panNumber
            QFOI.updatePanImage (Just encryptedPanNumber) (Just req.imageId) person.id
          Person.DRIVER -> do
            encryptedPanNumber <- encrypt req.panNumber
            DIQuery.updatePanNumber (Just encryptedPanNumber) person.id
          _ -> pure ()
  if DVRC.isNameCompareRequired transporterConfig verifyBy
    then Redis.withWaitOnLockRedisWithExpiry (DVRC.makeDocumentVerificationLockKey personId.getId) 10 10 runBody
    else runBody
  res <- case person.role of
    Person.DRIVER -> do
      fork "enabling driver if all the mandatory document is verified" $ do
        merchantOpCity <-
          CQMOC.findById merchantOpCityId
            >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
        driverImages <- IQuery.findAllByPersonId transporterConfig personId
        let driverImagesInfo = IQuery.DriverImagesInfo {driverId = personId, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
        let onlyMandatoryDocs = Just True
            shouldActivateRc = False
        void $ SStatus.statusHandler' (Just person) driverImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible person.id adminApprovalRequired (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId
    _ -> pure False
  pure res
  where
    callIdfy :: Person.Person -> Maybe DPan.DriverPanCard -> DVRC.DriverDocument -> DTC.TransporterConfig -> Flow APISuccess
    callIdfy person mdriverPanInformation driverDocument transporterConfig = do
      documentVerificationConfig <- QODC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId DTO.PanCard CAR Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show DTO.PanCard))
      image1 <- DVRC.getDocumentImage person.id req.imageId ODC.PanCard
      let extractReq =
            Verification.ExtractImageReq
              { image1 = image1,
                image2 = Nothing,
                driverId = person.id.getId
              }

      let validateExtractedPan resp = case resp.extractedPan of
            Just extractedPan -> do
              let extractedPanNo = removeSpaceAndDash <$> extractedPan.id_number
              let extractedNameOnCard = extractedPan.name_on_card
              logInfo ("extractedNameOnCard: " <> show extractedNameOnCard)
              logInfo ("req.panName: " <> show req.panName)
              if verifyBy == DPan.FRONTEND_SDK
                then case (req.panName, extractedNameOnCard) of
                  (Just providedName, Just extractedName) | not (T.null providedName) && not (T.null extractedName) -> do
                    let nameCompareReq =
                          Verification.NameCompareReq
                            { extractedName = extractedName,
                              verifiedName = providedName,
                              percentage = Just True,
                              driverId = person.id.getId
                            }
                    isValid <- DVRC.isNameComparePercentageValid person.merchantId merchantOpCityId nameCompareReq
                    unless isValid $ throwError (MismatchDataError "Provided name and extracted name on card do not match")
                    when documentVerificationConfig.doStrictVerifcation $ do
                      now <- getCurrentTime
                      let parsedDob = extractedPan.date_of_birth >>= DVRC.parseDateTime
                      verifyPanFlow person merchantOpCityId documentVerificationConfig (fromMaybe "" extractedPanNo) (fromMaybe now parsedDob) (Id req.imageId) extractedNameOnCard
                    unless isValid $ throwError (InvalidRequest "Name match failed")
                  _ -> throwError (InvalidRequest "PAN name is required")
                else
                  unless (extractedPanNo == Just req.panNumber) $
                    throwImageError (Id req.imageId) $
                      ImageDocumentNumberMismatch
                        (maybe "null" maskText extractedPanNo)
                        (maskText req.panNumber)
              pure extractedPan
            Nothing -> throwImageError (Id req.imageId) ImageExtractionFailed

      case mdriverPanInformation of
        Just driverPanInformation -> do
          let verificationStatus = driverPanInformation.verificationStatus
          when (verificationStatus == Documents.VALID) $
            throwError PanAlreadyLinked

          resp <- Verification.extractPanImage person.merchantId merchantOpCityId extractReq
          extractedPan <- validateExtractedPan resp
          when (DVRC.isNameCompareRequired transporterConfig verifyBy) $
            DVRC.validateDocument person.merchantId merchantOpCityId person.id extractedPan.name_on_card extractedPan.date_of_birth (Just req.panNumber) ODC.PanCard driverDocument
          DPQuery.updateVerificationStatus Documents.VALID person.id
        Nothing -> do
          resp <- Verification.extractPanImage person.merchantId merchantOpCityId extractReq
          extractedPan <- validateExtractedPan resp
          when (DVRC.isNameCompareRequired transporterConfig verifyBy) $
            DVRC.validateDocument person.merchantId merchantOpCityId person.id extractedPan.name_on_card extractedPan.date_of_birth (Just req.panNumber) ODC.PanCard driverDocument
          panCardDetails <- buildPanCard person extractedPan.pan_type extractedPan.name_on_card extractedPan.date_of_birth (Just verifyBy) (Id req.imageId) req.panNumber
          DPQuery.create panCardDetails

      pure Success

    checkIfGenuineReq :: (ServiceFlow m r) => API.Types.UI.DriverOnboardingV2.DriverPanReq -> Person.Person -> m ()
    checkIfGenuineReq API.Types.UI.DriverOnboardingV2.DriverPanReq {..} person = do
      (txnId, valStatus) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Cannot find necessary data for SDK response!!!!")) (return $ (,) <$> transactionId <*> validationStatus)
      hvResp <- Verification.verifySdkResp person.merchantId merchantOpCityId (VI.VerifySdkDataReq txnId)
      (respTxnId, respStatus, respUserDetails) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Invalid data recieved while validating data.")) (return $ (,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= txnId) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      when (Image.convertHVStatusToValidationStatus respStatus /= valStatus) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      case respUserDetails of
        VI.HVPanFlow (VI.PanFlow {pan = panFromResp, name = nameFromResp, dob = dobFromResp}) -> do
          panNum <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "PAN number not found in SDK validation response even though it's compulsory for Pan")) (return panFromResp)
          when (panNumber /= panNum) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (nameOnCard /= nameFromResp) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (isJust dateOfBirth && (formatUTCToDateString <$> dateOfBirth) /= (T.unpack <$> dobFromResp)) $ do
            logDebug $ "date of Birth and dob is : " <> show (formatUTCToDateString <$> dateOfBirth) <> " " <> show dobFromResp
            void $ Image.throwValidationError (Just imageId1) Nothing Nothing
        _ -> void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      where
        formatUTCToDateString :: UTCTime -> String
        formatUTCToDateString = formatTime defaultTimeLocale "%d-%m-%Y"

makeVerifyPanHitsCountKey :: Text -> Text
makeVerifyPanHitsCountKey panNumber = "VerifyPan:panNumberHits:" <> panNumber <> ":hitsCount"

castTextToDomainType :: Maybe Text -> Maybe DPan.PanType
castTextToDomainType panType = case panType of
  Just "Individual" -> Just DPan.INDIVIDUAL
  Just _ -> Just DPan.BUSINESS
  Nothing -> Nothing

verifyPanFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> DocumentVerificationConfig -> Text -> UTCTime -> Id Image.Image -> Maybe Text -> Flow ()
verifyPanFlow person merchantOpCityId documentVerificationConfig panNumber driverDateOfBirth imageId1 nameOnCard = do
  now <- getCurrentTime
  encryptedPan <- encrypt panNumber
  let imageExtractionValidation =
        if documentVerificationConfig.checkExtraction
          then Domain.Success
          else Domain.Skipped
  verifyRes <-
    Verification.verifyPanAsync person.merchantId merchantOpCityId $
      Verification.VerifyPanAsyncReq {panNumber, driverId = person.id.getId, dateOfBirth = driverDateOfBirth, fullName = fromMaybe "" nameOnCard}
  case verifyRes.requestor of
    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntity person imageId1 Nothing driverDateOfBirth Nothing nameOnCard verifyRes.requestId now imageExtractionValidation encryptedPan
    _ -> throwError $ InternalError ("Service provider not configured to return PAN verification async responses. Provider Name : " <> (show verifyRes.requestor))

onVerifyPan :: VerificationReqRecord -> VT.PanVerificationResponse -> VT.VerificationService -> Flow AckResponse
onVerifyPan verificationReq output serviceName = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  if verificationReq.imageExtractionValidation == Domain.Skipped
    && (output.dobMatch == Just False)
    then do
      case serviceName of
        VT.Idfy -> IVQuery.updateExtractValidationStatus Domain.Failed verificationReq.requestId
        _ -> throwError $ InternalError ("Unknown Service provider webhook encopuntered in onVerifyDL. Name of provider : " <> show serviceName)
      pure Ack
    else do
      onVerifyPanHandler person verificationReq.documentImageId1 verificationReq.documentImageId2 output
      pure Ack

onVerifyPanHandler :: Person.Person -> Id Image.Image -> Maybe (Id Image.Image) -> VT.PanVerificationResponse -> Flow ()
onVerifyPanHandler person imageId1 imageId2 output = do
  case output.inputDetails of
    Just details -> do
      mEncryptedPanNumber <- mapM encrypt (Just $ details.inputPanNumber)
      let isvalid = (&&) <$> output.dobMatch <*> output.nameMatch
      DPQuery.updateStrictVerificationStatusByDriverId isvalid person.id
      case person.role of
        role | DCommon.checkFleetOwnerRole role -> do
          QFOI.updatePanImage mEncryptedPanNumber (Just imageId1.getId) person.id
        Person.DRIVER -> do
          DIQuery.updatePanNumber mEncryptedPanNumber person.id
        _ -> pure ()
      (image1, image2) <- uncurry (liftA2 (,)) $ both (maybe (return Nothing) ImageQuery.findById) (Just imageId1, imageId2)
      when (((image1 >>= (.verificationStatus)) /= Just Documents.VALID) && ((image2 >>= (.verificationStatus)) /= Just Documents.VALID)) $
        mapM_ (maybe (return ()) (ImageQuery.updateVerificationStatusAndFailureReason Documents.VALID (ImageNotValid "verificationStatus updated to VALID by dashboard."))) [Just imageId1, imageId2]
    _ -> pure ()

buildPanCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe DPan.VerifiedBy -> Id Image.Image -> Text -> Flow DPan.DriverPanCard
buildPanCard person panType panName panDob verifyBy image1 panNumber = do
  panNoEnc <- encrypt panNumber
  now <- getCurrentTime
  uuid <- generateGUID
  let parsedDob = panDob >>= DVRC.parseDateTime
  return $
    DPan.DriverPanCard
      { panCardNumber = panNoEnc,
        documentImageId1 = image1,
        driverId = person.id,
        id = uuid,
        verificationStatus = Documents.VALID,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        consent = True,
        docType = castTextToDomainType panType,
        consentTimestamp = now,
        documentImageId2 = Nothing,
        driverDob = parsedDob,
        driverName = Just person.firstName,
        driverNameOnGovtDB = panName,
        failedRules = [],
        verifiedBy = verifyBy,
        isStrictlyVerified = Nothing
      }

mkIdfyVerificationEntity :: Person.Person -> Id Image.Image -> Maybe (Id Image.Image) -> UTCTime -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime -> Domain.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Flow Domain.IdfyVerification
mkIdfyVerificationEntity person imageId1 imageId2 driverDateOfBirth dateOfIssue nameOnCard requestId now imageExtractionValidation encryptedPan = do
  id <- generateGUID
  return $
    Domain.IdfyVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        requestId,
        imageExtractionValidation = imageExtractionValidation,
        documentNumber = encryptedPan,
        issueDateOnDoc = dateOfIssue,
        driverDateOfBirth = Just driverDateOfBirth,
        docType = DTO.PanCard,
        status = "pending",
        idfyResponse = Nothing,
        multipleRC = Nothing,
        retryCount = Just 0,
        nameOnCard,
        vehicleCategory = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        airConditioned = Nothing,
        oxygen = Nothing,
        ventilator = Nothing,
        createdAt = now,
        updatedAt = now
      }
