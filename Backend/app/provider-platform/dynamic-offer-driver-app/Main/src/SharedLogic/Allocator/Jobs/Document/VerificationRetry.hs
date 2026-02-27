{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Document.VerificationRetry
  ( retryDocumentVerificationJob,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as VehicleRegistrationCert
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.HyperVergeVerification as DHVVerification
import Domain.Types.IdfyVerification
import qualified Domain.Types.IdfyVerification as DIdfyVerification
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Beam.Functions as B
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow, ServiceFlow, VerificationFlow)
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.DocumentVerificationConfig as QODC
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Verification as Verification

retryDocumentVerificationJob ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadReader r m,
    HasKafkaProducer r,
    HasField "ttenTokenCacheExpiry" r Seconds,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  Job 'RetryDocumentVerification ->
  m ExecutionResult
retryDocumentVerificationJob jobDetails = withLogTag ("JobId-" <> jobDetails.id.getId) do
  let jobData = jobDetails.jobInfo.jobData
  verificationReq <- IVQuery.findByRequestId jobData.requestId >>= fromMaybeM (InternalError "Verification request not found")
  person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
  documentVerificationConfig <- QODC.findByMerchantOpCityIdAndDocumentTypeAndCategory person.merchantOperatingCityId verificationReq.docType (fromMaybe DVC.CAR verificationReq.vehicleCategory) Nothing >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show verificationReq.docType))
  let maxRetryCount = documentVerificationConfig.maxRetryCount
  if (fromMaybe 0 verificationReq.retryCount) <= maxRetryCount
    then do
      documentNum <- decrypt verificationReq.documentNumber
      IVQuery.updateStatus "source_down_retried" verificationReq.requestId
      case verificationReq.docType of
        DTO.VehicleRegistrationCertificate -> callVerifyRC documentNum person verificationReq
        DTO.DriverLicense -> callVerifyDL documentNum person verificationReq
        _ -> pure ()
    else do
      IVQuery.updateStatus "source_down_failed" verificationReq.requestId
  return Complete
  where
    callVerifyRC :: (VerificationFlow m r, HasField "ttenTokenCacheExpiry" r Seconds, SchedulerFlow r, ServiceFlow m r, HasField "blackListedJobs" r [Text], HasSchemaName SchedulerJobT, EsqDBReplicaFlow m r) => Text -> DP.Person -> DIdfyVerification.IdfyVerification -> m ()
    callVerifyRC documentNum person verificationReq = do
      verifyRes <-
        Verification.verifyRC person.merchantId person.merchantOperatingCityId Nothing (Verification.VerifyRCReq {rcNumber = documentNum, driverId = person.id.getId, token = Nothing, udinNo = Nothing, engineNumber = Nothing, chassisNumber = Nothing, applicantMobile = Nothing})
      case verifyRes.verifyRCResp of
        Verification.AsyncResp res -> do
          case res.requestor of
            VT.Idfy -> IVQuery.create =<< mkIdfyNewVerificationEntity verificationReq res.requestId
            VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperNewVergeVerificationEntity res.requestId verificationReq res.transactionId
            _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> T.pack (show res.requestor))
          CQO.setVerificationPriorityList person.id verifyRes.remPriorityList
        Verification.SyncResp res -> void $ VehicleRegistrationCert.onVerifyRC person Nothing res (Just verifyRes.remPriorityList) (Just verificationReq.imageExtractionValidation) (Just verificationReq.documentNumber) verificationReq.documentImageId1 (verificationReq.retryCount <&> (+ 1)) (Just "source_down_retrying") Nothing
    callVerifyDL :: VerificationFlow m r => Text -> DP.Person -> DIdfyVerification.IdfyVerification -> m ()
    callVerifyDL documentNum person verificationReq = do
      whenJust verificationReq.driverDateOfBirth $ \dob -> do
        verifyRes <-
          Verification.verifyDLAsync person.merchantId person.merchantOperatingCityId $
            Verification.VerifyDLAsyncReq {dlNumber = documentNum, dateOfBirth = dob, driverId = person.id.getId, returnState = Just True}
        case verifyRes.requestor of
          VT.Idfy -> IVQuery.create =<< mkIdfyNewVerificationEntity verificationReq verifyRes.requestId
          VT.HyperVergeRCDL -> HVQuery.create =<< mkHyperNewVergeVerificationEntity verifyRes.requestId verificationReq verifyRes.transactionId
          _ -> throwError $ InternalError ("Service provider not configured to return async responses. Provider Name : " <> (show verifyRes.requestor))

mkHyperNewVergeVerificationEntity :: MonadFlow m => Text -> DIdfyVerification.IdfyVerification -> Maybe Text -> m DHVVerification.HyperVergeVerification
mkHyperNewVergeVerificationEntity reqId DIdfyVerification.IdfyVerification {..} transactionId = do
  now <- getCurrentTime
  newId <- generateGUID
  return $
    DHVVerification.HyperVergeVerification
      { id = newId,
        retryCount = Just $ maybe 1 (1 +) retryCount,
        status = "source_down_retrying",
        hypervergeResponse = idfyResponse,
        requestId = reqId,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkIdfyNewVerificationEntity :: MonadFlow m => DIdfyVerification.IdfyVerification -> Text -> m DIdfyVerification.IdfyVerification
mkIdfyNewVerificationEntity verificationReq requestId = do
  now <- getCurrentTime
  newId <- generateGUID
  return $
    verificationReq
      { id = newId,
        retryCount = Just $ maybe 1 (1 +) verificationReq.retryCount,
        status = "source_down_retrying",
        requestId,
        createdAt = now,
        updatedAt = now
      }
