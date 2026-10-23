{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.Transfer where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Finance.Domain.Types.Transfer
import qualified Lib.Finance.Storage.Beam.Transfer as Beam

instance FromTType' Beam.Transfer Lib.Finance.Domain.Types.Transfer.Transfer where
  fromTType' (Beam.TransferT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.Transfer.Transfer
          { accountId = Kernel.Types.Id.Id accountId,
            amount = amount,
            attemptCount = attemptCount,
            counterAccountId = Kernel.Types.Id.Id <$> counterAccountId,
            coveredEntryIds = coveredEntryIds,
            createdAt = createdAt,
            currency = currency,
            direction = direction,
            entryIds = entryIds,
            feeAmount = feeAmount,
            holdEntryId = Kernel.Types.Id.Id <$> holdEntryId,
            id = Kernel.Types.Id.Id id,
            lastError = lastError,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            metadata = metadata >>= Kernel.Utils.JSON.valueToMaybe,
            railKey = railKey,
            railOrderId = railOrderId,
            railStatus = railStatus,
            referenceId = referenceId,
            referenceType = referenceType,
            requestedAmount = requestedAmount,
            scheduledFor = scheduledFor,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Transfer Lib.Finance.Domain.Types.Transfer.Transfer where
  toTType' (Lib.Finance.Domain.Types.Transfer.Transfer {..}) = do
    Beam.TransferT
      { Beam.accountId = Kernel.Types.Id.getId accountId,
        Beam.amount = amount,
        Beam.attemptCount = attemptCount,
        Beam.counterAccountId = Kernel.Types.Id.getId <$> counterAccountId,
        Beam.coveredEntryIds = coveredEntryIds,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.direction = direction,
        Beam.entryIds = entryIds,
        Beam.feeAmount = feeAmount,
        Beam.holdEntryId = Kernel.Types.Id.getId <$> holdEntryId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastError = lastError,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.metadata = Data.Aeson.toJSON <$> metadata,
        Beam.railKey = railKey,
        Beam.railOrderId = railOrderId,
        Beam.railStatus = railStatus,
        Beam.referenceId = referenceId,
        Beam.referenceType = referenceType,
        Beam.requestedAmount = requestedAmount,
        Beam.scheduledFor = scheduledFor,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
