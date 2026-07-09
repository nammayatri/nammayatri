{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.IdfyWebhook
  ( onVerify,
    idfyWebhookHandler,
    oldIdfyWebhookHandler,
    idfyWebhookV2Handler,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.CourtRecordCheck as CourtRecordCheck
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.GstVerification as GstCard
import qualified Domain.Action.UI.DriverOnboarding.PanVerification as PanCard
import qualified Domain.Action.UI.DriverOnboarding.UdyamVerification as UdyamCard
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.IdfyVerification as DIdfyVerification
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Idfy.WebhookHandler as Idfy
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.DriverOnboarding as SLogicOnboarding
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import Storage.Queries.Person as QP
import qualified Tools.Verification as Verification

-- FIXME this is temprorary solution for backward compatibility
oldIdfyWebhookHandler ::
  Maybe Text ->
  Value ->
  Flow AckResponse
oldIdfyWebhookHandler secret val = do
  logInfo $ "IdfyWebhook.oldIdfyWebhookHandler: received webhook hasSecret=" <> show (isJust secret)
  merchantServiceConfig <-
    CQMSC.findOne (DMSC.VerificationService Verification.Idfy)
      >>= fromMaybeM (InternalError "No verification service provider configured")
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> do
      case vsc of
        Verification.IdfyConfig idfyCfg -> do
          Idfy.webhookHandler idfyCfg onVerify secret val
        Verification.FaceVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.GovtDataConfig -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.DigiLockerConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.TtenVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy -> Tten verfication config"
        Verification.MorthConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.EkatraConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
    _ -> throwError $ InternalError "Unknown Service Config"

idfyWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Text ->
  Value ->
  Flow AckResponse
idfyWebhookHandler merchantShortId secret val = do
  logInfo $
    "IdfyWebhook.idfyWebhookHandler: received webhook merchantShortId=" <> merchantShortId.getShortId
      <> " hasSecret="
      <> show (isJust secret)
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant Nothing
  merchantServiceUsageConfig <-
    getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (CMSUC.findByMerchantOpCityId merchantOpCityId Nothing))
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Just merchantId.getId, serviceName = Just (DMSC.VerificationService merchantServiceUsageConfig.verificationService)}) (Just (maybeToList <$> CQMSC.findByServiceAndCity (DMSC.VerificationService merchantServiceUsageConfig.verificationService) merchantOpCityId))
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantId:" <> merchantId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> do
      case vsc of
        Verification.IdfyConfig idfyCfg -> do
          Idfy.webhookHandler idfyCfg onVerify secret val
        Verification.FaceVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.GovtDataConfig -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.DigiLockerConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.TtenVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy -> Tten verfication config"
        Verification.MorthConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.EkatraConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
    _ -> throwError $ InternalError "Unknown Service Config"

idfyWebhookV2Handler ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Value ->
  Flow AckResponse
idfyWebhookV2Handler merchantShortId opCity secret val = do
  logInfo $
    "IdfyWebhook.idfyWebhookV2Handler: received webhook merchantShortId=" <> merchantShortId.getShortId
      <> " opCity="
      <> show opCity
      <> " hasSecret="
      <> show (isJust secret)
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantServiceUsageConfig <-
    getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (CMSUC.findByMerchantOpCityId merchantOpCityId Nothing))
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Just merchantId.getId, serviceName = Just (DMSC.VerificationService merchantServiceUsageConfig.verificationService)}) (Just (maybeToList <$> CQMSC.findByServiceAndCity (DMSC.VerificationService merchantServiceUsageConfig.verificationService) merchantOpCityId))
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantId:" <> merchantId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> do
      case vsc of
        Verification.IdfyConfig idfyCfg -> do
          Idfy.webhookHandler idfyCfg onVerify secret val
        Verification.FaceVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.GovtDataConfig -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.DigiLockerConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.TtenVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy -> Tten verfication config"
        Verification.MorthConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
        Verification.EkatraConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
    _ -> throwError $ InternalError "Unknown Service Config"

onVerify :: Idfy.VerificationResponse -> Text -> Flow AckResponse
onVerify (Idfy.VerificationResponse rsp) respDump = do
  -- Log only safe metadata; the raw payload (rsp/respDump) can carry PII and is already
  -- persisted to the verification request via IVQuery.updateResponse below.
  logInfo $
    "IdfyWebhook.onVerify: received webhook requestId=" <> rsp.request_id
      <> " groupId="
      <> rsp.group_id
      <> " action="
      <> rsp.action
      <> " status="
      <> rsp.status
  case rsp.result of
    Just (Idfy.CRCResult resSrcOp) -> do
      logInfo $ "IdfyWebhook.onVerify: CRC result -> storing court record for driverId(group_id)=" <> rsp.group_id
      CourtRecordCheck.onVerifyCRC (Id rsp.group_id) resSrcOp.source_output
      return Ack
    _
      | rsp._type == "ind_court_record" -> do
        -- Failed CRC (no CRCResult) has no idfy_verification row; record on driver_identity_info instead of throwing.
        logWarning $ "IdfyWebhook.onVerify: CRC callback without result for driverId(group_id)=" <> rsp.group_id <> " status=" <> rsp.status
        CourtRecordCheck.onVerifyCRCError (Id rsp.group_id) ("Idfy CRC failed: status=" <> rsp.status)
        return Ack
      | otherwise -> do
        verificationReq <- IVQuery.findByRequestId rsp.request_id >>= fromMaybeM (InternalError $ "Verification request not found for requestId=" <> rsp.request_id)
        person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
        logInfo $
          "IdfyWebhook.onVerify: looked up verificationReq requestId=" <> rsp.request_id
            <> " driverId="
            <> verificationReq.driverId.getId
            <> " docType="
            <> verificationReq.docType
            <> " status="
            <> verificationReq.status
        IVQuery.updateResponse rsp.status (Just respDump) rsp.request_id
        let resultStatus = getResultStatus rsp.result
        logInfo $ "IdfyWebhook.onVerify: parsed resultStatus=" <> show resultStatus
        if resultStatus == Just "source_down"
          then do
            logInfo $ "IdfyWebhook.onVerify: source_down path requestId=" <> rsp.request_id
            handleIdfySourceDown person scheduleRetryVerificationJob verificationReq
            return Ack
          else do
            mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId
            logInfo $ "IdfyWebhook.onVerify: routing to verifyDocument mbRemPriorityList=" <> show mbRemPriorityList
            ack_ <- maybe (pure Ack) (flip (verifyDocument person verificationReq) mbRemPriorityList) rsp.result
            logInfo $ "IdfyWebhook.onVerify: completed requestId=" <> rsp.request_id
            void $ SStatus.processStatusEvent (Just person) Nothing (SStatus.PersonDocChangedEvent verificationReq.driverId)
            return ack_
  where
    getResultStatus :: Maybe Idfy.IdfyResult -> Maybe Text
    getResultStatus mbResult =
      case mbResult of
        Just (Idfy.DLResult (Idfy.SourceOutput o)) -> o.status
        Just (Idfy.PanResult (Idfy.SourceOutput o)) -> o.status
        Just (Idfy.GstResult (Idfy.SourceOutput o)) -> o.status
        Just (Idfy.UdyamAadhaarResult (Idfy.SourceOutput o)) -> o.status
        Just (Idfy.RCResult (Idfy.ExtractionOutput o)) -> o.status
        _ -> Nothing
    verifyDocument person verificationReq rslt mbRemPriorityList = do
      verificationReqRecord <- fromMaybeM (InternalError $ "Non-document idfy_verification row routed to webhook, requestId=" <> verificationReq.requestId <> ", docType=" <> verificationReq.docType) (SLogicOnboarding.makeIdfyVerificationReqRecord verificationReq)
      case rslt of
        Idfy.RCResult resExtOp ->
          RC.onVerifyRC
            person
            (Just verificationReqRecord)
            (Idfy.convertRCOutputToRCVerificationResponse resExtOp.extraction_output)
            mbRemPriorityList
            (Just verificationReq.imageExtractionValidation)
            (Just verificationReq.documentNumber)
            verificationReq.documentImageId1
            verificationReq.retryCount
            (Just verificationReq.status)
            (Just VT.Idfy)
            Nothing
        Idfy.DLResult resSrcOp ->
          DL.onVerifyDL
            verificationReqRecord
            (Idfy.convertDLOutputToDLVerificationOutput resSrcOp.source_output)
            VT.Idfy
        Idfy.PanResult resSrcOp ->
          PanCard.onVerifyPan
            verificationReqRecord
            (Idfy.convertPanOutputToPanVerification resSrcOp.source_output)
            VT.Idfy
        Idfy.GstResult resSrcOp -> do
          logInfo $
            "IdfyWebhook.verifyDocument: GstResult branch requestId=" <> verificationReq.requestId
              <> " driverId="
              <> verificationReq.driverId.getId
              <> " sourceOutputStatus="
              <> show (resSrcOp.source_output.status)
          GstCard.onVerifyGst
            verificationReqRecord
            (Idfy.convertGstOutputToGstVerification resSrcOp.source_output)
            VT.Idfy
        Idfy.BankAccountResult _ -> pure Ack
        Idfy.PanAadhaarLinkResult resSrcOp ->
          PanCard.onVerifyPanAadhaarLink
            verificationReqRecord
            (Idfy.convertPanAadhaarLinkOutputToPanAadhaarLinkVerification resSrcOp.source_output)
            VT.Idfy
        Idfy.UdyamAadhaarResult resSrcOp ->
          UdyamCard.onVerifyUdyam
            verificationReqRecord
            (Idfy.convertUdyamAadhaarOutputToUdyamAadhaarVerification resSrcOp.source_output)
            VT.Idfy
        _ -> pure Ack

handleIdfySourceDown :: DP.Person -> (IV.IdfyVerification -> Flow ()) -> DIdfyVerification.IdfyVerification -> Flow ()
handleIdfySourceDown person retryFunc verificationReq = do
  unless (verificationReq.docType == docTypeToText DVC.VehicleRegistrationCertificate) $ retryFunc verificationReq
  mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId >>= \mbpl -> if mbpl == Just [] then return Nothing else return mbpl
  rcNum <- decrypt verificationReq.documentNumber
  flip (maybe (retryFunc verificationReq)) mbRemPriorityList $
    \priorityList -> do
      logDebug $ "Idfy Source down trying with alternate service providers remaining !!!!!!" <> verificationReq.requestId
      rsltresp' <- try @_ @SomeException $ Verification.verifyRC person.merchantId person.merchantOperatingCityId True (Just priorityList) verificationReq.vehicleCategory (Verification.VerifyRCReq {rcNumber = rcNum, driverId = verificationReq.driverId.getId, token = Nothing, udinNo = Nothing, engineNumber = Nothing, chassisNumber = Nothing, applicantMobile = Nothing})
      case rsltresp' of
        Left _ -> retryFunc verificationReq
        Right resp' -> do
          case resp'.verifyRCResp of
            Verification.AsyncResp res -> do
              now <- getCurrentTime
              case res.requestor of
                VT.Idfy -> IVQuery.create =<< SLogicOnboarding.mkRCIdfyVerificationEntity person res.requestId now verificationReq.imageExtractionValidation verificationReq.documentNumber verificationReq.issueDateOnDoc verificationReq.vehicleCategory verificationReq.airConditioned verificationReq.oxygen verificationReq.ventilator verificationReq.documentImageId1 Nothing Nothing
                VT.HyperVergeRCDL -> HVQuery.create =<< RC.mkHyperVergeVerificationEntity person res.requestId now verificationReq.imageExtractionValidation verificationReq.documentNumber verificationReq.issueDateOnDoc verificationReq.vehicleCategory verificationReq.airConditioned verificationReq.oxygen verificationReq.ventilator verificationReq.documentImageId1 Nothing Nothing res.transactionId
                _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> T.pack (show res.requestor))
              CQO.setVerificationPriorityList person.id resp'.remPriorityList
            Verification.SyncResp res -> void $ RC.onVerifyRC person Nothing res.response (Just resp'.remPriorityList) (Just verificationReq.imageExtractionValidation) (Just verificationReq.documentNumber) verificationReq.documentImageId1 verificationReq.retryCount (Just verificationReq.status) Nothing verificationReq.vehicleCategory

scheduleRetryVerificationJob :: IV.IdfyVerification -> Flow ()
scheduleRetryVerificationJob verificationReq = do
  logDebug $ "Idfy Source down and no remaining service providers left in priority list, scheduling a job for future retry for requestId : " <> verificationReq.requestId
  let scheduleTime = calculateScheduleTime (fromMaybe 0 verificationReq.retryCount)
  createJobIn @_ @'RetryDocumentVerification verificationReq.merchantId verificationReq.merchantOperatingCityId scheduleTime $
    RetryDocumentVerificationJobData
      { requestId = verificationReq.requestId
      }
  where
    calculateScheduleTime retryCount = do
      let retryInterval = 60 * 60 -- 1 hour
      let retryTime = retryInterval * (3 ^ retryCount)
      retryTime
