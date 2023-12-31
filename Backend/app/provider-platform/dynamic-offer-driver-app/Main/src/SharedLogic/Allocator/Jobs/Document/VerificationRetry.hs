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

import Domain.Types.DriverOnboarding.IdfyVerification
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as DTO
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.Merchant.OnboardingDocumentConfig as QODC
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Verification as Verification

retryDocumentVerificationJob ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'RetryDocumentVerification ->
  m ExecutionResult
retryDocumentVerificationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  verificationReq <- IVQuery.findByRequestId jobData.requestId >>= fromMaybeM (InternalError "Verification request not found")
  person <- runInReplica $ QP.findById verificationReq.driverId >>= fromMaybeM (PersonDoesNotExist verificationReq.driverId.getId)
  onboardingDocumentConfig <- QODC.findByMerchantOpCityIdAndDocumentType person.merchantOperatingCityId (castDoctype verificationReq.docType) >>= fromMaybeM (OnboardingDocumentConfigNotFound person.merchantOperatingCityId.getId (show verificationReq.docType))
  let maxRetryCount = onboardingDocumentConfig.maxRetryCount
  if verificationReq.retryCount <= maxRetryCount
    then do
      documentNumber <- decrypt verificationReq.documentNumber
      IVQuery.updateStatus verificationReq.requestId "source_down_retried"
      case verificationReq.docType of
        Image.VehicleRegistrationCertificate -> do
          verifyRes <-
            Verification.verifyRCAsync person.merchantId person.merchantOperatingCityId $
              Verification.VerifyRCAsyncReq {rcNumber = documentNumber, driverId = person.id.getId}
          mkNewVerificationEntity verificationReq verifyRes.requestId
        Image.DriverLicense -> do
          whenJust verificationReq.driverDateOfBirth $ \dob -> do
            verifyRes <-
              Verification.verifyDLAsync person.merchantId person.merchantOperatingCityId $
                Verification.VerifyDLAsyncReq {dlNumber = documentNumber, dateOfBirth = dob, driverId = person.id.getId}
            mkNewVerificationEntity verificationReq verifyRes.requestId
    else do
      IVQuery.updateStatus verificationReq.requestId "source_down_failed"
  return Complete
  where
    castDoctype :: Image.ImageType -> DTO.DocumentType
    castDoctype docType =
      case docType of
        Image.VehicleRegistrationCertificate -> DTO.RC
        Image.DriverLicense -> DTO.DL

    mkNewVerificationEntity verificationReq requestId = do
      now <- getCurrentTime
      newId <- generateGUID
      let newVerificationReq =
            verificationReq
              { id = newId,
                retryCount = verificationReq.retryCount + 1,
                status = "source_down_retrying",
                requestId,
                createdAt = now,
                updatedAt = now
              }
      IVQuery.create newVerificationReq
