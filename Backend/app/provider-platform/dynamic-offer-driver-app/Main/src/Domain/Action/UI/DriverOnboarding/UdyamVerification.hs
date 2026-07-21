{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is distributed under the terms of the GNU Affero General Public License.
-}

module Domain.Action.UI.DriverOnboarding.UdyamVerification
  ( DriverUdyamReq (..),
    DriverUdyamRes,
    verifyUdyam,
    verifyUdyamFlow,
    verifyUdyamFlowWithRequestor,
    onVerifyUdyam,
  )
where

import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DFR
import qualified Domain.Types.DocumentAuditLog as DAL
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverUdyam as DUdyam
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.Image as Image
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.DriverOnboarding (VerificationReqRecord, getDriverDocumentInfo)
import qualified SharedLogic.DriverOnboarding.Audit as Audit
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverUdyam as DUQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as PersonQuery
import Tools.Error
import qualified Tools.Verification as Verification

data DriverUdyamReq = DriverUdyamReq
  { uamNumber :: Text,
    imageId1 :: Id Image.Image
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverUdyamRes = APISuccess

makeVerifyUdyamHitsCountKey :: Text -> Text
makeVerifyUdyamHitsCountKey uamNumber = "VerifyUdyam:uamNumberHits:" <> uamNumber <> ":hitsCount"

verifyUdyam ::
  -- | Audit actor. 'Nothing' ⇒ the driver/fleet-owner verifying their own Udyam; 'Just' ⇒ a dashboard operator.
  Maybe Audit.Requestor ->
  (Id Person.Person, Id DMOC.MerchantOperatingCity) ->
  DriverUdyamReq ->
  Flow Bool
verifyUdyam mbRequestor (personId, merchantOpCityId) req = do
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyUdyamHitsCountKey req.uamNumber) externalServiceRateLimitOptions

  person <- PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  (blocked, _driverDocument) <- getDriverDocumentInfo person
  when blocked $ throwError AccountBlocked
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  case transporterConfig.allowDuplicateUdyam of
    Just False -> do
      udyamHash <- getDbHash req.uamNumber
      udyamInfoList <- DUQuery.findAllByEncryptedUdyamNumber udyamHash
      let otherDriverIds = filter (/= person.id) (map (.driverId) udyamInfoList)
      unless (Kernel.Prelude.null otherDriverIds) $ do
        otherPersonDetails <- PersonQuery.getDriversByIdIn otherDriverIds
        when (person.role `elem` map (.role) otherPersonDetails) $ throwError UdyamAlreadyLinked
    _ -> pure ()
  cfg <- SStatus.getFleetDocVerificationConfig merchantOpCityId ODC.UDYAMCertificate person.role
  if cfg.doStrictVerifcation
    then verifyUdyamFlowWithRequestor (Audit.auditActorFromPersonOrRequestor person mbRequestor) person merchantOpCityId req.uamNumber req.imageId1
    else upsertManualUdyamRecord mbRequestor person req.uamNumber req.imageId1
  res <- case person.role of
    Person.DRIVER -> do
      fork "enabling driver if all the mandatory document is verified" $ do
        void $ SStatus.processStatusEvent (Just person) (Just transporterConfig) (SStatus.PersonDocChangedEvent person.id)
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible mbRequestor person.id Nothing (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId (Just transporterConfig)
    _ -> pure False
  pure res

-- Backward-compatible wrapper: kept with the old signature because dashboard
-- Management.DriverRegistration also calls it without an audit requestor; the audit
-- actor then falls back to the document owner acting on their own behalf.
verifyUdyamFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> Text -> Id Image.Image -> Flow ()
verifyUdyamFlow person = verifyUdyamFlowWithRequestor (Audit.auditActorFromPersonOrRequestor person Nothing) person

verifyUdyamFlowWithRequestor :: Audit.Requestor -> Person.Person -> Id DMOC.MerchantOperatingCity -> Text -> Id Image.Image -> Flow ()
verifyUdyamFlowWithRequestor requestor person merchantOpCityId uamNumber imageId1 = do
  now <- getCurrentTime
  encryptedUam <- encrypt uamNumber
  let imageExtractionValidation = DIdfy.Skipped
  verifyRes <-
    Verification.verifyUdyamAadhaarAsync person.merchantId merchantOpCityId $
      Verification.VerifyUdyamAadhaarAsyncReq {uamNumber, driverId = person.id.getId}
  case verifyRes.requestor of
    VT.Idfy -> do
      IVQuery.create =<< mkIdfyVerificationEntityUdyam person imageId1 verifyRes.requestId now imageExtractionValidation encryptedUam
      -- Async dispatch mutates no Udyam/image row yet: record a request-time row (eventId = provider
      -- requestId) so the webhook result can be correlated, mirroring DL/RC/Aadhaar/Bank flows.
      Audit.auditImageStatusByIdWithEvent requestor (Audit.entityTypeFromRole person.role) person.id.getId "Udyam" imageId1 Nothing Documents.PENDING DAL.VERIFICATION_REQUESTED (Just verifyRes.requestId) person.merchantId person.merchantOperatingCityId
    -- Adding an async provider branch? Extend pullSourcesFor + hvWorkflowHint in SyncVerificationStatus.
    _ -> throwError $ InternalError ("Service provider not configured to return Udyam Aadhaar verification async responses. Provider Name : " <> show verifyRes.requestor)
  pure ()

onVerifyUdyam :: VerificationReqRecord -> VT.UdyamAadhaarVerificationResponse -> VT.VerificationService -> Flow AckResponse
onVerifyUdyam verificationReq output serviceName = do
  case serviceName of
    VT.Idfy -> do
      person <- PersonQuery.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
      mDriverUdyam <- DUQuery.findByDriverId person.id
      now <- getCurrentTime
      case mDriverUdyam of
        Just driverUdyam -> do
          let updated =
                driverUdyam
                  { DUdyam.verificationStatus = Documents.VALID,
                    DUdyam.documentImageId = verificationReq.documentImageId1,
                    DUdyam.enterpriseName = output.enterpriseName,
                    DUdyam.enterpriseType = output.enterpriseType,
                    DUdyam.rejectReason = Nothing,
                    DUdyam.updatedAt = now
                  }
          DUQuery.updateByPrimaryKey updated
          Audit.auditDocStatus Audit.externalProvider (if DCommon.checkFleetOwnerRole person.role then DAL.FLEET_OWNER else DAL.DRIVER) person.id.getId "Udyam" DAL.DRIVER_UDYAM (Just updated.id.getId) (Just (show driverUdyam.verificationStatus)) (Just (show updated.verificationStatus)) DAL.STATUS_CHANGED Nothing person.merchantId person.merchantOperatingCityId
        Nothing -> do
          udyamCardDetails <- buildDriverUdyamCard person verificationReq.documentNumber output.enterpriseName output.enterpriseType Documents.VALID verificationReq.documentImageId1
          DUQuery.create udyamCardDetails
          Audit.auditDocStatus Audit.externalProvider (if DCommon.checkFleetOwnerRole person.role then DAL.FLEET_OWNER else DAL.DRIVER) person.id.getId "Udyam" DAL.DRIVER_UDYAM (Just udyamCardDetails.id.getId) Nothing (Just (show udyamCardDetails.verificationStatus)) DAL.STATUS_CHANGED Nothing person.merchantId person.merchantOperatingCityId
      case person.role of
        role
          | DCommon.checkFleetOwnerRole role ->
            void $ DFR.enableFleetIfPossible (Just Audit.externalProvider) person.id Nothing (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId Nothing
        _ -> pure ()
    _ -> throwError $ InternalError ("Unknown Service provider webhook encountered in onVerifyUdyam. Name of provider : " <> show serviceName)
  pure Ack

upsertManualUdyamRecord :: Maybe Audit.Requestor -> Person.Person -> Text -> Id Image.Image -> Flow ()
upsertManualUdyamRecord mbRequestor person uamNumber imageId = do
  encryptedUam <- encrypt uamNumber
  existing <- DUQuery.findByDriverId person.id
  now <- getCurrentTime
  case existing of
    Just driverUdyam -> do
      let updated =
            driverUdyam
              { DUdyam.udyamNumber = encryptedUam,
                DUdyam.documentImageId = imageId,
                DUdyam.verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
                DUdyam.rejectReason = Nothing,
                DUdyam.enterpriseName = Nothing,
                DUdyam.enterpriseType = Nothing,
                DUdyam.updatedAt = now
              }
      DUQuery.updateByPrimaryKey updated
      Audit.auditDocStatus (Audit.auditActorFromPersonOrRequestor person mbRequestor) (if DCommon.checkFleetOwnerRole person.role then DAL.FLEET_OWNER else DAL.DRIVER) person.id.getId "Udyam" DAL.DRIVER_UDYAM (Just updated.id.getId) (Just (show driverUdyam.verificationStatus)) (Just (show updated.verificationStatus)) DAL.STATUS_CHANGED Nothing person.merchantId person.merchantOperatingCityId
    Nothing -> do
      udyamCardDetails <- buildDriverUdyamCard person encryptedUam Nothing Nothing Documents.MANUAL_VERIFICATION_REQUIRED imageId
      DUQuery.create udyamCardDetails
      Audit.auditDocStatus (Audit.auditActorFromPersonOrRequestor person mbRequestor) (if DCommon.checkFleetOwnerRole person.role then DAL.FLEET_OWNER else DAL.DRIVER) person.id.getId "Udyam" DAL.DRIVER_UDYAM (Just udyamCardDetails.id.getId) Nothing (Just (show udyamCardDetails.verificationStatus)) DAL.STATUS_CHANGED Nothing person.merchantId person.merchantOperatingCityId

buildDriverUdyamCard :: Person.Person -> EncryptedHashedField 'AsEncrypted Text -> Maybe Text -> Maybe Text -> Documents.VerificationStatus -> Id Image.Image -> Flow DUdyam.DriverUdyam
buildDriverUdyamCard person encryptedUdyamNumber enterpriseName enterpriseType verificationStatus imageId = do
  now <- getCurrentTime
  uuid <- generateGUID
  return $
    DUdyam.DriverUdyam
      { driverId = person.id,
        documentImageId = imageId,
        id = Id uuid :: Id DUdyam.DriverUdyam,
        udyamNumber = encryptedUdyamNumber,
        verificationStatus = verificationStatus,
        verifiedBy = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        enterpriseName = enterpriseName,
        enterpriseType = enterpriseType,
        rejectReason = Nothing,
        createdAt = now,
        updatedAt = now
      }

mkIdfyVerificationEntityUdyam :: MonadFlow m => Person.Person -> Id Image.Image -> Text -> UTCTime -> DIdfy.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> m DIdfy.IdfyVerification
mkIdfyVerificationEntityUdyam person imageId1 requestId now imageExtractionValidation encryptedUam = do
  entityId <- generateGUID
  return $
    DIdfy.IdfyVerification
      { id = Id entityId,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = Nothing,
        requestId,
        docType = docTypeToText ODC.UDYAMCertificate,
        documentNumber = encryptedUam,
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
