{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.GstVerification
  ( DriverGstinReq (..),
    DriverGstinRes,
    verifyGstin,
    onVerifyGst,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import Data.Aeson hiding (Success)
import Data.Text as T hiding (elem, find, length, map, null, zip)
import Data.Tuple.Extra (both)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DFR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverGstin as DGst
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (find)
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
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverGstin as DGQuery
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Tools.Error
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

data DriverGstinReq = DriverGstinReq
  { gstin :: Text,
    imageId :: Text, --Image,
    driverId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverGstinRes = APISuccess

verifyGstin ::
  DPan.VerifiedBy ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverGstinReq ->
  Maybe Bool ->
  Bool ->
  Flow Bool
verifyGstin verifyBy mbMerchant (personId, _, merchantOpCityId) req adminApprovalRequired isDashboard = do
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyGstinHitsCountKey req.gstin) externalServiceRateLimitOptions

  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  (blocked, driverDocument) <- DVRC.getDriverDocumentInfo person
  when blocked $ throwError AccountBlocked
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  case transporterConfig.allowDuplicateGst of
    Just False -> do
      gstinHash <- getDbHash req.gstin
      gstInfoList <- DGQuery.findAllByEncryptedGstNumber gstinHash
      when (length gstInfoList > 1) $ throwError GstAlreadyLinked
      gstPersonDetails <- Person.getDriversByIdIn (map (.driverId) gstInfoList)
      let getRoles = map (.role) gstPersonDetails
      when (person.role `elem` getRoles) $ throwError GstAlreadyLinked
    _ -> pure ()
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbGstVerificationService =
        (if isDashboard then merchantServiceUsageConfig.dashboardGstVerificationService else merchantServiceUsageConfig.gstVerificationService)
  let runBody = do
        case mbGstVerificationService of
          Just VI.Idfy -> do
            mdriverGstInformation <- DGQuery.findByDriverId person.id
            void $ callIdfy person mdriverGstInformation driverDocument transporterConfig
          _ -> do
            gstCardDetails <- buildGstinCard person Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            DGQuery.create gstCardDetails

        case person.role of
          role | DCommon.checkFleetOwnerRole role -> do
            gstin <- encrypt req.gstin
            QFOI.updateGstImage (Just gstin) (Just req.imageId) person.id
          _ -> pure ()
  if DVRC.isNameCompareRequired transporterConfig verifyBy
    then Redis.withWaitOnLockRedisWithExpiry (DVRC.makeDocumentVerificationLockKey personId.getId) 10 10 runBody
    else runBody
  res <- case person.role of
    Person.DRIVER -> do
      now <- getCurrentTime
      fork "enabling driver if all the mandatory document is verified" $ do
        merchantOpCity <-
          CQMOC.findById merchantOpCityId
            >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
        let entity = IQuery.PersonEntity person
        entityImages <- IQuery.findAllByEntityId transporterConfig entity
        let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity = merchantOpCity, entityImages, transporterConfig, now}
        let onlyMandatoryDocs = Just True
            shouldActivateRc = False
        void $ SStatus.statusHandler' person entityImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible person.id adminApprovalRequired (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId
    _ -> pure False
  pure res
  where
    buildGstinCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DGst.DriverGstin
    buildGstinCard person address constitution_of_business date_of_liability is_provisional legal_name trade_name type_of_registration valid_from valid_upto pan_number = do
      gstinEnc <- encrypt req.gstin
      now <- getCurrentTime
      uuid <- generateGUID
      return $
        DGst.DriverGstin
          { documentImageId1 = Id req.imageId,
            driverId = person.id,
            id = uuid,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            address = address,
            constitutionOfBusiness = constitution_of_business,
            updatedAt = now,
            documentImageId2 = Nothing,
            dateOfLiability = date_of_liability >>= DVRC.parseDateTime,
            driverName = Just person.firstName,
            gstin = gstinEnc,
            isProvisional = is_provisional,
            panNumber = pan_number,
            legalName = legal_name,
            tradeName = trade_name,
            typeOfRegistration = type_of_registration,
            validFrom = valid_from >>= DVRC.parseDateTime,
            validUpto = valid_upto >>= DVRC.parseDateTime,
            verifiedBy = pure verifyBy
          }

    callIdfy :: Person.Person -> Maybe DGst.DriverGstin -> DVRC.DriverDocument -> DTC.TransporterConfig -> Flow APISuccess
    callIdfy person mdriverGstInformation driverDocument transporterConfig = do
      documentVerificationConfig <-
        CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId ODC.GSTCertificate Nothing
          >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.GSTCertificate))
            . listToMaybe
      image1 <- DVRC.getDocumentImage person.id req.imageId ODC.GSTCertificate
      let extractReq =
            Verification.ExtractImageReq
              { image1 = image1,
                image2 = Nothing,
                driverId = person.id.getId
              }

      let validateExtractedGst resp = case resp.extractedGST of
            Just extractedGST -> do
              let extractedGstNo = removeSpaceAndDash <$> extractedGST.gstin
              unless (extractedGstNo == Just req.gstin) $
                throwImageError (Id req.imageId) $
                  ImageDocumentNumberMismatch
                    (maybe "null" maskText extractedGstNo)
                    (maskText req.gstin)
              verifyGstFlow person merchantOpCityId documentVerificationConfig (fromMaybe "" extractedGstNo) (Id req.imageId)
              pure extractedGST
            Nothing -> throwImageError (Id req.imageId) ImageExtractionFailed

      case mdriverGstInformation of
        Just driverGstInformation -> do
          let verificationStatus = driverGstInformation.verificationStatus
          when (verificationStatus == Documents.VALID) $
            throwError GstAlreadyLinked

          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          extractedGst <- validateExtractedGst resp
          when (DVRC.isNameCompareRequired transporterConfig verifyBy) $
            DVRC.validateDocument person.merchantId merchantOpCityId person.id Nothing Nothing extractedGst.pan_number ODC.GSTCertificate driverDocument
          DGQuery.updateVerificationStatus Documents.MANUAL_VERIFICATION_REQUIRED person.id
        Nothing -> do
          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          extractedGst <- validateExtractedGst resp
          when (DVRC.isNameCompareRequired transporterConfig verifyBy) $
            DVRC.validateDocument person.merchantId merchantOpCityId person.id Nothing Nothing extractedGst.pan_number ODC.GSTCertificate driverDocument
          gstCardDetails <- buildGstinCard person extractedGst.address extractedGst.constitution_of_business extractedGst.date_of_liability extractedGst.is_provisional extractedGst.legal_name extractedGst.trade_name extractedGst.type_of_registration extractedGst.valid_from extractedGst.valid_upto extractedGst.pan_number
          DGQuery.create gstCardDetails
      pure Success

verifyGstFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> ODC.DocumentVerificationConfig -> Text -> Id Image.Image -> Flow ()
verifyGstFlow person merchantOpCityId documentVerificationConfig gstNumber imageId1 = do
  now <- getCurrentTime
  encryptedGst <- encrypt gstNumber
  let imageExtractionValidation =
        if documentVerificationConfig.checkExtraction
          then DIdfy.Success
          else DIdfy.Skipped
  verifyRes <-
    Verification.verifyGstAsync person.merchantId merchantOpCityId $
      Verification.VerifyGstAsyncReq {gstNumber, driverId = person.id.getId, filingDetails = True, eInvoiceDetails = True}
  case verifyRes.requestor of
    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntityGst person imageId1 verifyRes.requestId now imageExtractionValidation encryptedGst
    _ -> throwError $ InternalError ("Service provider not configured to return GST verification async responses. Provider Name : " <> (show verifyRes.requestor))
  pure ()

onVerifyGst :: VerificationReqRecord -> VT.GstVerificationResponse -> VT.VerificationService -> Flow AckResponse
onVerifyGst verificationReq output serviceName = do
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  if verificationReq.imageExtractionValidation == Domain.Skipped
    && (output.gstinStatus /= Just "Active")
    then do
      case serviceName of
        VT.Idfy -> do
          IVQuery.updateExtractValidationStatus Domain.Failed verificationReq.requestId
          DGQuery.updateVerificationStatus Documents.INVALID verificationReq.driverId
        _ -> throwError $ InternalError ("Unknown Service provider webhook encountered in onVerifyGst. Name of provider : " <> show serviceName)
      pure Ack
    else do
      onVerifyGstHandler person verificationReq.documentImageId1 verificationReq.documentImageId2 output
      pure Ack

onVerifyGstHandler :: Person.Person -> Id Image.Image -> Maybe (Id Image.Image) -> VT.GstVerificationResponse -> Flow ()
onVerifyGstHandler person imageId1 imageId2 output = do
  mEncryptedGstinNumber <- encrypt `mapM` output.gstin
  let isValidGst = output.gstinStatus == Just "Active"
  DGQuery.updateVerificationStatus (if isValidGst then Documents.VALID else Documents.INVALID) person.id
  when (DCommon.checkFleetOwnerRole person.role) $ do
    QFOI.updateGstImage mEncryptedGstinNumber (Just imageId1.getId) person.id
  (image1, image2) <- uncurry (liftA2 (,)) $ both (maybe (return Nothing) ImageQuery.findById) (Just imageId1, imageId2)
  when (((image1 >>= (.verificationStatus)) /= Just Documents.VALID) && ((image2 >>= (.verificationStatus)) /= Just Documents.VALID)) $
    mapM_ (maybe (return ()) (ImageQuery.updateVerificationStatusAndFailureReason Documents.VALID (ImageNotValid "verificationStatus updated to VALID by dashboard."))) [Just imageId1, imageId2]

mkIdfyVerificationEntityGst :: MonadFlow m => Person.Person -> Id Image.Image -> Text -> UTCTime -> DIdfy.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> m DIdfy.IdfyVerification
mkIdfyVerificationEntityGst person imageId1 requestId now imageExtractionValidation encryptedGst = do
  entityId <- generateGUID
  return $
    DIdfy.IdfyVerification
      { id = entityId,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = Nothing,
        requestId,
        docType = ODC.GSTCertificate,
        documentNumber = encryptedGst,
        driverDateOfBirth = Nothing,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = Nothing,
        status = "pending",
        idfyResponse = Nothing,
        vehicleCategory = Nothing,
        airConditioned = Nothing,
        oxygen = Nothing,
        ventilator = Nothing,
        retryCount = Just 0,
        nameOnCard = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

makeVerifyGstinHitsCountKey :: Text -> Text
makeVerifyGstinHitsCountKey gstin = "VerifyGstin:gstinHits:" <> gstin <> ":hitsCount"
