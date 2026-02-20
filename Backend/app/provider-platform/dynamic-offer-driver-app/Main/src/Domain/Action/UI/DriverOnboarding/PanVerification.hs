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
  )
where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as DO
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import qualified Control.Monad.Extra as CME
import Data.Aeson hiding (Success)
import Data.Text as T hiding (elem, find, length, map, zip)
import Data.Time (defaultTimeLocale, formatTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DFR
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.External.Verification.Interface as VI
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Image as IQuery
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
            panCardDetails <- buildPanCard person Nothing Nothing Nothing
            DPQuery.create panCardDetails
          Just VI.Idfy -> do
            void $ callIdfy person mdriverPanInformation driverDocument transporterConfig
          _ -> do
            panCardDetails <- buildPanCard person Nothing Nothing Nothing
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
        let driverImagesInfo = IQuery.DriverImagesInfo {driverId = Just personId, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
        let onlyMandatoryDocs = Just True
            shouldActivateRc = False
        void $ SStatus.statusHandler' person driverImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible person.id adminApprovalRequired (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId
    _ -> pure False
  pure res
  where
    callIdfy :: Person.Person -> Maybe DPan.DriverPanCard -> DVRC.DriverDocument -> DTC.TransporterConfig -> Flow APISuccess
    callIdfy person mdriverPanInformation driverDocument transporterConfig = do
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
              when (verifyBy /= DPan.FRONTEND_SDK) $
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
          panCardDetails <- buildPanCard person extractedPan.pan_type extractedPan.name_on_card extractedPan.date_of_birth
          DPQuery.create panCardDetails

      pure Success

    buildPanCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DPan.DriverPanCard
    buildPanCard person panType panName panDob = do
      panNoEnc <- encrypt req.panNumber
      now <- getCurrentTime
      uuid <- generateGUID
      let parsedDob = panDob >>= DVRC.parseDateTime
      return $
        DPan.DriverPanCard
          { panCardNumber = panNoEnc,
            documentImageId1 = Id req.imageId,
            driverId = person.id,
            id = uuid,
            verificationStatus = Documents.VALID,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
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
            verifiedBy = Just verifyBy
          }

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

    castTextToDomainType :: Maybe Text -> Maybe DPan.PanType
    castTextToDomainType panType = case panType of
      Just "Individual" -> Just DPan.INDIVIDUAL
      Just _ -> Just DPan.BUSINESS
      Nothing -> Nothing

    makeVerifyPanHitsCountKey :: Text -> Text
    makeVerifyPanHitsCountKey panNumber = "VerifyPan:panNumberHits:" <> panNumber <> ":hitsCount"
