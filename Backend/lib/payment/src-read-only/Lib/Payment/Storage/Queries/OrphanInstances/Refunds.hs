{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.OrphanInstances.Refunds where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Lib.Payment.Storage.Beam.Refunds as Beam

instance FromTType' Beam.Refunds Lib.Payment.Domain.Types.Refunds.Refunds where
  fromTType' (Beam.RefundsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.Refunds.Refunds
          { createdAt = createdAt,
            errorCode = errorCode,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            idAssignedByServiceProvider = idAssignedByServiceProvider,
            initiatedBy = initiatedBy,
            merchantId = merchantId,
            orderId = Kernel.Types.Id.Id orderId,
            refundAmount = refundAmount,
            shortId = shortId,
            split = split >>= Kernel.Utils.JSON.valueToMaybe,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Refunds Lib.Payment.Domain.Types.Refunds.Refunds where
  toTType' (Lib.Payment.Domain.Types.Refunds.Refunds {..}) = do
    Beam.RefundsT
      { Beam.createdAt = createdAt,
        Beam.errorCode = errorCode,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idAssignedByServiceProvider = idAssignedByServiceProvider,
        Beam.initiatedBy = initiatedBy,
        Beam.merchantId = merchantId,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.refundAmount = refundAmount,
        Beam.shortId = shortId,
        Beam.split = split >>= Just . Data.Aeson.toJSON,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
