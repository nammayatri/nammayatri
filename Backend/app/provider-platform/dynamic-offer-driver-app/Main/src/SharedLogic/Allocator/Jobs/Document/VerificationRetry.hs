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

import qualified Domain.Types.DocumentVerificationConfig as DTO
import Domain.Types.IdfyVerification
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.DocumentVerificationConfig as QODC
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Verification as Verification

retryDocumentVerificationJob ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    KvDbFlow m r
  ) =>
  Job 'RetryDocumentVerification ->
  m ExecutionResult
retryDocumentVerificationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  verificationReq <- IVQuery.findByRequestId jobData.requestId >>= fromMaybeM (InternalError "Verification request not found")
  person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
  documentVerificationConfig <- QODC.findByMerchantOpCityIdAndDocumentTypeAndCategory person.merchantOperatingCityId verificationReq.docType (fromMaybe Vehicle.CAR verificationReq.vehicleCategory) >>= fromMaybeM (DocumentVerificationConfigNotFound person.merchantOperatingCityId.getId (show verificationReq.docType))
  let maxRetryCount = documentVerificationConfig.maxRetryCount
  if (fromMaybe 0 verificationReq.retryCount) <= maxRetryCount
    then do
      documentNumber <- decrypt verificationReq.documentNumber
      IVQuery.updateStatus "source_down_retried" verificationReq.requestId
      case verificationReq.docType of
        DTO.VehicleRegistrationCertificate -> do
          verifyRes <-
            Verification.verifyRC person.merchantId person.merchantOperatingCityId
              Verification.VerifyRCReq {rcNumber = documentNumber, driverId = person.id.getId}
          case verifyRes of
            Verification.AsyncResp Verification.VerifyAsyncResp {..} -> do
              mkNewVerificationEntity verificationReq requestId
            _ -> pure ()
        DTO.DriverLicense -> do
          whenJust verificationReq.driverDateOfBirth $ \dob -> do
            verifyRes <-
              Verification.verifyDLAsync person.merchantId person.merchantOperatingCityId $
                Verification.VerifyDLAsyncReq {dlNumber = documentNumber, dateOfBirth = dob, driverId = person.id.getId}
            mkNewVerificationEntity verificationReq verifyRes.requestId
        _ -> pure ()
    else do
      IVQuery.updateStatus "source_down_failed" verificationReq.requestId
  return Complete
  where
    mkNewVerificationEntity verificationReq requestId = do
      now <- getCurrentTime
      newId <- generateGUID
      let newVerificationReq =
            verificationReq
              { id = newId,
                retryCount = Just $ maybe 1 ((+) 1) verificationReq.retryCount,
                status = "source_down_retrying",
                requestId,
                createdAt = now,
                updatedAt = now
              }
      IVQuery.create newVerificationReq
