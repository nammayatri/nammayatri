{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RefundRequest where

import qualified Domain.Types.RefundRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RefundRequest as Beam

instance FromTType' Beam.RefundRequest Domain.Types.RefundRequest.RefundRequest where
  fromTType' (Beam.RefundRequestT {..}) = do
    pure $
      Just
        Domain.Types.RefundRequest.RefundRequest
          { code = code,
            currency = currency,
            description = description,
            evidenceS3Path = evidenceS3Path,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            orderId = Kernel.Types.Id.Id orderId,
            personId = Kernel.Types.Id.Id personId,
            refundPurpose = refundPurpose,
            refundsAmount = refundsAmount,
            refundsId = Kernel.Types.Id.Id <$> refundsId,
            refundsTries = refundsTries,
            requestedAmount = requestedAmount,
            responseDescription = responseDescription,
            status = status,
            transactionAmount = transactionAmount,
            transactionId = Kernel.Types.Id.Id transactionId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RefundRequest Domain.Types.RefundRequest.RefundRequest where
  toTType' (Domain.Types.RefundRequest.RefundRequest {..}) = do
    Beam.RefundRequestT
      { Beam.code = code,
        Beam.currency = currency,
        Beam.description = description,
        Beam.evidenceS3Path = evidenceS3Path,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.refundPurpose = refundPurpose,
        Beam.refundsAmount = refundsAmount,
        Beam.refundsId = Kernel.Types.Id.getId <$> refundsId,
        Beam.refundsTries = refundsTries,
        Beam.requestedAmount = requestedAmount,
        Beam.responseDescription = responseDescription,
        Beam.status = status,
        Beam.transactionAmount = transactionAmount,
        Beam.transactionId = Kernel.Types.Id.getId transactionId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
