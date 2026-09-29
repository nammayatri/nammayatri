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
            excludedCount = excludedCount,
            executionDate = executionDate,
            failureCode = failureCode,
            failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            itemCount = itemCount,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            nextStatusCallAt = nextStatusCallAt,
            origin = origin,
            partnerBatchRef = partnerBatchRef,
            payoutRail = payoutRail,
            payoutServiceName = payoutServiceName,
            resolvedAt = resolvedAt,
            status = status,
            statusCheckCalls = statusCheckCalls,
            statusCheckRound = statusCheckRound,
            statusNoDataReplies = statusNoDataReplies,
            submittedAt = submittedAt,
            totalAmount = totalAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutBatch Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch where
  toTType' (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch {..}) = do
    Beam.PayoutBatchT
      { Beam.clientRefNo = clientRefNo,
        Beam.createdAt = createdAt,
        Beam.excludedCount = excludedCount,
        Beam.executionDate = executionDate,
        Beam.failureCode = failureCode,
        Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.itemCount = itemCount,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.nextStatusCallAt = nextStatusCallAt,
        Beam.origin = origin,
        Beam.partnerBatchRef = partnerBatchRef,
        Beam.payoutRail = payoutRail,
        Beam.payoutServiceName = payoutServiceName,
        Beam.resolvedAt = resolvedAt,
        Beam.status = status,
        Beam.statusCheckCalls = statusCheckCalls,
        Beam.statusCheckRound = statusCheckRound,
        Beam.statusNoDataReplies = statusNoDataReplies,
        Beam.submittedAt = submittedAt,
        Beam.totalAmount = totalAmount,
        Beam.updatedAt = updatedAt
      }
