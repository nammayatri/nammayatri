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
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Environment
import Kernel.Beam.Functions
import qualified Kernel.External.Verification.Idfy.WebhookHandler as Idfy
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
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
    CQMSUC.findByMerchantOpCityId merchantOpCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.VerificationService merchantServiceUsageConfig.verificationService)
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantId:" <> merchantId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> do
      case vsc of
        Verification.IdfyConfig idfyCfg -> do
          Idfy.webhookHandler idfyCfg onVerify secret val
        Verification.FaceVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
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
    CQMSUC.findByMerchantOpCityId merchantOpCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.VerificationService merchantServiceUsageConfig.verificationService)
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantId:" <> merchantId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> do
      case vsc of
        Verification.IdfyConfig idfyCfg -> do
          Idfy.webhookHandler idfyCfg onVerify secret val
        Verification.FaceVerificationConfig _ -> throwError $ InternalError "Incorrect service config for Idfy"
    _ -> throwError $ InternalError "Unknown Service Config"

onVerify :: Idfy.VerificationResponse -> Text -> Flow AckResponse
onVerify resp respDump = do
  verificationReq <- IVQuery.findByRequestId resp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  IVQuery.updateResponse resp.request_id resp.status respDump
  let resultStatus = getResultStatus resp.result
  if resultStatus == (Just "source_down")
    then do
      scheduleRetryVerificationJob verificationReq
      return Ack
    else do
      ack_ <- maybe (pure Ack) (verifyDocument verificationReq) resp.result
      person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
      -- running statusHandler to enable Driver
      void $ Status.statusHandler (verificationReq.driverId, person.merchantId, person.merchantOperatingCityId) verificationReq.multipleRC
      return ack_
  where
    getResultStatus mbResult = mbResult >>= (\rslt -> (rslt.extraction_output >>= (.status)) <|> (rslt.source_output >>= (.status)))
    verifyDocument verificationReq rslt
      | isJust rslt.extraction_output =
        maybe (pure Ack) (RC.onVerifyRC verificationReq) rslt.extraction_output
      | isJust rslt.source_output =
        maybe (pure Ack) (DL.onVerifyDL verificationReq) rslt.source_output
      | otherwise = pure Ack

scheduleRetryVerificationJob :: IV.IdfyVerification -> Flow ()
scheduleRetryVerificationJob verificationReq = do
  maxShards <- asks (.maxShards)
  let scheduleTime = calculateScheduleTime verificationReq.retryCount
  createJobIn @_ @'RetryDocumentVerification scheduleTime maxShards $
    RetryDocumentVerificationJobData
      { requestId = verificationReq.requestId
      }
  where
    calculateScheduleTime retryCount = do
      let retryInterval = 60 * 60 -- 1 hour
      let retryTime = retryInterval * (3 ^ retryCount)
      retryTime
