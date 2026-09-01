{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.OrphanInstances.PayoutBatch where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutBatch
import qualified Lib.Payment.Storage.Beam.PayoutBatch as Beam

instance FromTType' Beam.PayoutBatch Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch where
  fromTType' (Beam.PayoutBatchT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch
          { clientRefNo = clientRefNo,
            createdAt = createdAt,
            failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            inquiryAttemptsToday = inquiryAttemptsToday,
            inquiryQuotaDate = inquiryQuotaDate,
            itemCount = itemCount,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            nextInquiryAt = nextInquiryAt,
            origin = origin,
            partnerBatchRef = partnerBatchRef,
            partnerResponseCode = partnerResponseCode,
            payoutRail = payoutRail,
            pendingCount = pendingCount,
            processedCount = processedCount,
            rejectedCount = rejectedCount,
            resolvedAt = resolvedAt,
            retryOfBatchId = Kernel.Types.Id.Id <$> retryOfBatchId,
            runId = runId,
            status = status,
            submittedAt = submittedAt,
            totalAmount = totalAmount,
            updatedAt = updatedAt,
            valueDate = valueDate
          }

instance ToTType' Beam.PayoutBatch Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch where
  toTType' (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch {..}) = do
    Beam.PayoutBatchT
      { Beam.clientRefNo = clientRefNo,
        Beam.createdAt = createdAt,
        Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.inquiryAttemptsToday = inquiryAttemptsToday,
        Beam.inquiryQuotaDate = inquiryQuotaDate,
        Beam.itemCount = itemCount,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.nextInquiryAt = nextInquiryAt,
        Beam.origin = origin,
        Beam.partnerBatchRef = partnerBatchRef,
        Beam.partnerResponseCode = partnerResponseCode,
        Beam.payoutRail = payoutRail,
        Beam.pendingCount = pendingCount,
        Beam.processedCount = processedCount,
        Beam.rejectedCount = rejectedCount,
        Beam.resolvedAt = resolvedAt,
        Beam.retryOfBatchId = Kernel.Types.Id.getId <$> retryOfBatchId,
        Beam.runId = runId,
        Beam.status = status,
        Beam.submittedAt = submittedAt,
        Beam.totalAmount = totalAmount,
        Beam.updatedAt = updatedAt,
        Beam.valueDate = valueDate
      }
