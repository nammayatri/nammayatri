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

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
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
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
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
    _ -> throwError $ InternalError "Unknown Service Config"

idfyWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Text ->
  Value ->
  Flow AckResponse
idfyWebhookHandler merchantShortId secret val = do
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant Nothing
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService merchantServiceUsageConfig.verificationService) merchantOpCityId
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
    _ -> throwError $ InternalError "Unknown Service Config"

idfyWebhookV2Handler ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Value ->
  Flow AckResponse
idfyWebhookV2Handler merchantShortId opCity secret val = do
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService merchantServiceUsageConfig.verificationService) merchantOpCityId
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
    _ -> throwError $ InternalError "Unknown Service Config"

onVerify :: Idfy.VerificationResponse -> Text -> Flow AckResponse
onVerify rsp respDump = do
  verificationReq <- IVQuery.findByRequestId rsp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
  IVQuery.updateResponse rsp.status (Just respDump) rsp.request_id
  let resultStatus = getResultStatus rsp.result
  if resultStatus == Just "source_down"
    then do
      handleIdfySourceDown person scheduleRetryVerificationJob verificationReq
      return Ack
    else do
      mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId
      ack_ <- maybe (pure Ack) (flip (verifyDocument person verificationReq) mbRemPriorityList) rsp.result
      -- running statusHandler to enable Driver
      void $ Status.statusHandler (verificationReq.driverId, person.merchantId, person.merchantOperatingCityId) (Just True) verificationReq.multipleRC Nothing Nothing
      return ack_
  where
    getResultStatus mbResult = mbResult >>= (\rslt -> (rslt.extraction_output >>= (.status)) <|> (rslt.source_output >>= (.status)))
    verifyDocument person verificationReq rslt mbRemPriorityList
      | isJust rslt.extraction_output =
        maybe (pure Ack) (\resExtOp -> RC.onVerifyRC person (Just (makeVerificationReqRecord verificationReq)) (castToVerificationRes resExtOp) mbRemPriorityList (Just verificationReq.imageExtractionValidation) (Just verificationReq.documentNumber) verificationReq.multipleRC verificationReq.documentImageId1 verificationReq.retryCount (Just verificationReq.status)) rslt.extraction_output
      | isJust rslt.source_output =
        maybe (pure Ack) (DL.onVerifyDL verificationReq) rslt.source_output
      | otherwise = pure Ack

    handleIdfySourceDown :: DP.Person -> (IV.IdfyVerification -> Flow ()) -> DIdfyVerification.IdfyVerification -> Flow ()
    handleIdfySourceDown person retryFunc verificationReq = do
      mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId
      rcNum <- decrypt verificationReq.documentNumber
      flip (maybe (retryFunc verificationReq)) mbRemPriorityList $
        \priorityList -> do
          logDebug $ "Idfy Source down trying with alternate service providers remaining !!!!!!" <> verificationReq.requestId
          resp' <- Verification.verifyRC person.merchantId person.merchantOperatingCityId (Just priorityList) $ Verification.VerifyRCReq {rcNumber = rcNum, driverId = verificationReq.driverId.getId}
          case resp'.verifyRCResp of
            Verification.AsyncResp res -> do
              now <- getCurrentTime
              case res.requestor of
                VT.Idfy -> IVQuery.create =<< RC.mkIdfyVerificationEntity person res.requestId now verificationReq.imageExtractionValidation verificationReq.multipleRC verificationReq.documentNumber verificationReq.issueDateOnDoc verificationReq.vehicleCategory verificationReq.airConditioned verificationReq.oxygen verificationReq.ventilator verificationReq.documentImageId1 Nothing Nothing
                VT.HyperVerge -> HVQuery.create =<< RC.mkHyperVergeVerificationEntity person res.requestId now verificationReq.imageExtractionValidation verificationReq.multipleRC verificationReq.documentNumber verificationReq.issueDateOnDoc verificationReq.vehicleCategory verificationReq.airConditioned verificationReq.oxygen verificationReq.ventilator verificationReq.documentImageId1 Nothing Nothing
                _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> T.pack (show res.requestor))
              CQO.setVerificationPriorityList person.id resp'.remPriorityList
            Verification.SyncResp res -> void $ RC.onVerifyRC person Nothing res (Just resp'.remPriorityList) (Just verificationReq.imageExtractionValidation) (Just verificationReq.documentNumber) verificationReq.multipleRC verificationReq.documentImageId1 verificationReq.retryCount (Just verificationReq.status)

    makeVerificationReqRecord IV.IdfyVerification {..} =
      RC.VerificationReqRecord
        { id = id.getId,
          verificaitonResponse = idfyResponse,
          ..
        }

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

castToVerificationRes :: Idfy.RCVerificationOutput -> VT.RCVerificationResponse
castToVerificationRes output =
  VT.RCVerificationResponse
    { registrationDate = output.registration_date,
      registrationNumber = output.registration_number,
      fitnessUpto = output.fitness_upto,
      insuranceValidity = output.insurance_validity,
      vehicleClass = output.vehicle_class,
      vehicleCategory = output.vehicle_category,
      seatingCapacity = output.seating_capacity,
      manufacturer = output.manufacturer,
      permitValidityFrom = output.permit_validity_from,
      permitValidityUpto = output.permit_validity_upto,
      pucValidityUpto = output.puc_validity_upto,
      manufacturerModel = output.manufacturer_model,
      mYManufacturing = output.m_y_manufacturing,
      colour = output.colour,
      color = output.color,
      fuelType = output.fuel_type,
      bodyType = output.body_type,
      status = output.status
    }
