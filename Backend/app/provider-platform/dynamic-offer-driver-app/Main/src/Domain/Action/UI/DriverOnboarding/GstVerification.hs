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
    assertUploadedGstLinkedToExistingPan,
    assertUploadedPanLinkedToExistingGst,
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
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.FleetOwnerInformation as FOI
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
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Cac.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverGstin as DGQuery
import qualified Storage.Queries.DriverGstinExtra as DGQueryExtra
import qualified Storage.Queries.DriverPanCard as DPQuery
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Tools.Error
import qualified Tools.Utils as Utils
import qualified Tools.Verification as Verification

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
  (blocked, driverDocument) <- getDriverDocumentInfo person
  when blocked $ throwError AccountBlocked
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  case transporterConfig.allowDuplicateGst of
    Just False -> do
      gstinHash <- getDbHash req.gstin
      gstInfoList <- DGQuery.findAllByEncryptedGstNumber gstinHash
      when (length gstInfoList > 1) $ do
        logError $
          "GstVerification: GSTIN already linked with multiple drivers; refusing duplicate. driverId="
            <> person.id.getId
            <> " merchantOpCityId="
            <> merchantOpCityId.getId
            <> " gstin="
            <> maskText req.gstin
            <> " existingCount="
            <> show (length gstInfoList)
            <> " existingDriverIds="
            <> show (map ((.getId) . (.driverId)) gstInfoList)
        throwError (GstDuplicateAcrossDrivers person.id.getId)
      when (not (fromMaybe False transporterConfig.allowGstReupload)) $ do
        gstPersonDetails <- Person.getDriversByIdIn (map (.driverId) gstInfoList)
        let getRoles = map (.role) gstPersonDetails
        when (person.role `elem` getRoles) $ do
          logError $
            "GstVerification: GSTIN already linked with another driver of the same role. driverId="
              <> person.id.getId
              <> " role="
              <> show person.role
              <> " merchantOpCityId="
              <> merchantOpCityId.getId
              <> " gstin="
              <> maskText req.gstin
              <> " conflictingDriverIds="
              <> show (map ((.getId) . (.id)) gstPersonDetails)
          throwError (GstInUseBySameRoleDriver person.id.getId)
    _ -> pure ()
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  merchantServiceUsageConfig <-
    getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (CMSUC.findByMerchantOpCityId merchantOpCityId Nothing))
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbGstVerificationService =
        (if isDashboard then merchantServiceUsageConfig.dashboardGstVerificationService else merchantServiceUsageConfig.gstVerificationService)
  let gstPanLinkCheckRequired = DCommon.checkFleetOwnerRole person.role && fromMaybe False transporterConfig.isGstPanLinkCheckRequired
  let runBody = do
        -- Inside the lock so the read of the existing PAN and the create of this GST
        -- are atomic w.r.t. a concurrent PAN upload for the same person (TOCTOU).
        when gstPanLinkCheckRequired $
          assertUploadedGstLinkedToExistingPan person req.gstin (Id req.imageId)
        case mbGstVerificationService of
          Just VI.Idfy -> do
            mdriverGstInformation <- DGQuery.findByDriverId person.id
            void $ callIdfy person mdriverGstInformation driverDocument transporterConfig
          _ -> do
            gstCardDetails <- buildGstinCard person Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            DGQuery.create gstCardDetails

        case person.role of
          role | DCommon.checkFleetOwnerRole role -> do
            gstin <- encrypt req.gstin
            QFOI.updateGstImage (Just gstin) (Just req.imageId) person.id
            -- GST upload is the first business-fleet signal; promote so later role-gated checks are correct (register API resets if individual).
            when (person.role == Person.FLEET_OWNER) $ do
              Person.updatePersonRole person.id Person.FLEET_BUSINESS
              QFOI.updateFleetType FOI.BUSINESS_FLEET person.id
          _ -> pure ()
  if isNameCompareRequired transporterConfig verifyBy || gstPanLinkCheckRequired
    then Redis.withWaitOnLockRedisWithExpiry (makeDocumentVerificationLockKey personId.getId) 10 10 runBody
    else runBody
  -- GST upload promoted FLEET_OWNER -> FLEET_BUSINESS in the DB above, but person.role is still stale in memory.
  let enablementRole = if person.role == Person.FLEET_OWNER then Person.FLEET_BUSINESS else person.role
  res <- case enablementRole of
    Person.DRIVER -> do
      fork "enabling driver if all the mandatory document is verified" $ do
        void $ SStatus.runRefreshOnboardingFlagsDriver (Just person) (Just transporterConfig) person.id
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible person.id adminApprovalRequired (DFR.castRoleToFleetType enablementRole) person.merchantOperatingCityId (Just transporterConfig)
    _ -> pure False
  pure res
  where
    buildGstinCard :: Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DGst.DriverGstin
    buildGstinCard person address constitution_of_business date_of_liability is_provisional legal_name trade_name type_of_registration valid_from valid_upto pan_number mbPincode mbStateName = do
      gstinEnc <- encrypt req.gstin
      now <- getCurrentTime
      uuid <- generateGUID
      return $
        DGst.DriverGstin
          { documentImageId1 = Id req.imageId,
            driverId = person.id,
            id = uuid,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            rejectReason = Nothing,
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
            verifiedBy = pure verifyBy,
            pincode = mbPincode,
            stateName = mbStateName
          }

    callIdfy :: Person.Person -> Maybe DGst.DriverGstin -> DriverDocument -> DTC.TransporterConfig -> Flow APISuccess
    callIdfy person mdriverGstInformation driverDocument transporterConfig = do
      logDebug $ "callIdfy: " <> show req
      checkExtraction <-
        if DCommon.checkFleetOwnerRole person.role
          then do
            fodvc <-
              getOneConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just ODC.GSTCertificate, role = Nothing}) (Just (maybeToList <$> CQFODVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId ODC.GSTCertificate Nothing))
                >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.GSTCertificate))
            pure fodvc.checkExtraction
          else do
            dvc <-
              listToMaybe <$> getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just ODC.GSTCertificate, vehicleCategory = Nothing}) (Just (CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId ODC.GSTCertificate Nothing))
                >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.GSTCertificate))
            pure dvc.checkExtraction
      image1 <- getValidDocumentImage person.id req.imageId ODC.GSTCertificate
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
              verifyGstFlow person merchantOpCityId checkExtraction (fromMaybe "" extractedGstNo) (Id req.imageId)
              pure extractedGST
            Nothing -> throwImageError (Id req.imageId) ImageExtractionFailed

      case mdriverGstInformation of
        Just driverGstInformation -> do
          let verificationStatus = driverGstInformation.verificationStatus
          when (verificationStatus == Documents.VALID && not (fromMaybe False transporterConfig.allowGstReupload)) $ do
            logError $
              "GstVerification: driver already has a VALID GST document; refusing re-verification. driverId="
                <> person.id.getId
                <> " merchantOpCityId="
                <> merchantOpCityId.getId
                <> " existingGstDocId="
                <> driverGstInformation.id.getId
                <> " requestedGstin="
                <> maskText req.gstin
            throwError (DriverGstAlreadyVerified person.id.getId)
          when (verificationStatus == Documents.INVALID && (transporterConfig.enableBotFlow == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True)) $ DGQuery.updateVerificationStatus Documents.PENDING person.id

          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          extractedGst <- validateExtractedGst resp
          when (isNameCompareRequired transporterConfig verifyBy) $
            validateDocument person.merchantId merchantOpCityId person.id Nothing Nothing extractedGst.pan_number ODC.GSTCertificate driverDocument
          DGQuery.updateVerificationStatus Documents.MANUAL_VERIFICATION_REQUIRED person.id
        Nothing -> do
          resp <- Verification.extractGSTImage person.merchantId merchantOpCityId extractReq
          extractedGst <- validateExtractedGst resp
          when (isNameCompareRequired transporterConfig verifyBy) $
            validateDocument person.merchantId merchantOpCityId person.id Nothing Nothing extractedGst.pan_number ODC.GSTCertificate driverDocument
          gstCardDetails <- buildGstinCard person extractedGst.address extractedGst.constitution_of_business extractedGst.date_of_liability extractedGst.is_provisional extractedGst.legal_name extractedGst.trade_name extractedGst.type_of_registration extractedGst.valid_from extractedGst.valid_upto extractedGst.pan_number Nothing Nothing
          DGQuery.create gstCardDetails
      pure Success

verifyGstFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Id Image.Image -> Flow ()
verifyGstFlow person merchantOpCityId checkExtraction gstNumber imageId1 = do
  logDebug $ "verifyGstFlow: " <> show gstNumber
  now <- getCurrentTime
  encryptedGst <- encrypt gstNumber
  let imageExtractionValidation =
        if checkExtraction
          then DIdfy.Success
          else DIdfy.Skipped
  verifyRes <-
    Verification.verifyGstAsync person.merchantId merchantOpCityId $
      Verification.VerifyGstAsyncReq {gstNumber, driverId = person.id.getId, filingDetails = True, eInvoiceDetails = True}
  logDebug $ "verifyRes: " <> show verifyRes
  case verifyRes.requestor of
    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntityGst person imageId1 verifyRes.requestId now imageExtractionValidation encryptedGst
    -- Adding an async provider branch? Extend pullSourcesFor + hvWorkflowHint in SyncVerificationStatus.
    _ -> throwError $ InternalError ("Service provider not configured to return GST verification async responses. Provider Name : " <> (show verifyRes.requestor))
  pure ()

onVerifyGst :: VerificationReqRecord -> VT.GstVerificationResponse -> VT.VerificationService -> Flow AckResponse
onVerifyGst verificationReq output serviceName = do
  logDebug $ "onVerifyGst: verificationReqId: " <> verificationReq.id <> ", driverId: " <> verificationReq.driverId.getId <> ", docType: " <> show verificationReq.docType <> ", status: " <> verificationReq.status
  logDebug $ "output: " <> show output
  logDebug $ "serviceName: " <> show serviceName
  person <- Person.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  if verificationReq.imageExtractionValidation == Domain.Skipped
    && (output.gstinStatus /= Just "Active")
    then do
      logDebug $ "onVerifyGst: imageExtractionValidation == Domain.Skipped && output.gstinStatus /= Just \"Active\""
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
  logDebug $ "onVerifyGstHandler: " <> show output
  mEncryptedGstinNumber <- encrypt `mapM` output.gstin
  let isActive = output.gstinStatus == Just "Active"
      isFleet = DCommon.checkFleetOwnerRole person.role
      mbLegalName = (\t -> if T.strip t == "" then Nothing else Just t) =<< output.legalName
      legalNameMissing = isFleet && isNothing mbLegalName
      verificationStatus = if isActive && not legalNameMissing then Documents.VALID else Documents.INVALID
      mbPrincipalAddr = output.principalPlaceOfBusinessFields
  DGQueryExtra.updateVerificationStatusWithPlaceDetails verificationStatus (mbPrincipalAddr >>= (.pincode)) (mbPrincipalAddr >>= (.stateName)) person.id
  when isFleet $ do
    QFOI.updateGstImage mEncryptedGstinNumber (Just imageId1.getId) person.id
    when (verificationStatus == Documents.VALID) $ QFOI.updateFleetName mbLegalName person.id
  (image1, image2) <- uncurry (liftA2 (,)) $ both (maybe (return Nothing) ImageQuery.findById) (Just imageId1, imageId2)
  let rejectForMissingLegalName = ImageQuery.updateVerificationStatusAndFailureReason Documents.INVALID GstLegalNameNotFound imageId1
      promoteImagesToValid =
        forM_ [(image1, Just imageId1), (image2, imageId2)] $ \(mbImg, mbImgId) ->
          unless ((mbImg >>= (.verificationStatus)) `elem` [Just Documents.VALID, Just Documents.INVALID]) $
            whenJust mbImgId $ ImageQuery.updateVerificationStatusAndFailureReason Documents.VALID (ImageNotValid "verificationStatus updated to VALID by dashboard.")
      invalidateImages =
        forM_ [(image1, Just imageId1), (image2, imageId2)] $ \(mbImg, mbImgId) ->
          unless ((mbImg >>= (.verificationStatus)) == Just Documents.INVALID) $
            whenJust mbImgId $ ImageQuery.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid "GST verification failed")
  if legalNameMissing
    then rejectForMissingLegalName
    else if verificationStatus == Documents.VALID then promoteImagesToValid else invalidateImages

assertUploadedGstLinkedToExistingPan :: Person.Person -> Text -> Id Image.Image -> Flow ()
assertUploadedGstLinkedToExistingPan person gstin imageId = do
  mPanCard <- DPQuery.findValidByDriverId person.id
  whenJust mPanCard $ \panCard -> do
    panNumber <- decrypt panCard.panCardNumber
    rejectIfPanGstUnlinked person panNumber gstin imageId

assertUploadedPanLinkedToExistingGst :: Person.Person -> Text -> Id Image.Image -> Flow ()
assertUploadedPanLinkedToExistingGst person panNumber imageId = do
  mDriverGstin <- DGQuery.findValidByDriverId person.id
  whenJust mDriverGstin $ \driverGstin -> do
    gstin <- decrypt driverGstin.gstin
    rejectIfPanGstUnlinked person panNumber gstin imageId

rejectIfPanGstUnlinked :: Person.Person -> Text -> Text -> Id Image.Image -> Flow ()
rejectIfPanGstUnlinked person panNumber gstin imageId = do
  let normalizedPan = T.toUpper (removeSpaceAndDash panNumber)
      normalizedGstin = T.toUpper (removeSpaceAndDash gstin)
      -- A GSTIN embeds the holder's PAN at characters 3-12, so the PAN must sit at that exact slice
      panInGstin = T.take 10 (T.drop 2 normalizedGstin)
  unless (panInGstin == normalizedPan) $ do
    logWarning $
      "rejectIfPanGstUnlinked: PAN not linked to GST, rejecting upload. driverId="
        <> person.id.getId
        <> " pan="
        <> maskText panNumber
        <> " gstin="
        <> maskText gstin
    Utils.cleanupUploadedImages [imageId] person.id
    throwError PanGstNumberMismatch

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
        docType = docTypeToText ODC.GSTCertificate,
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
