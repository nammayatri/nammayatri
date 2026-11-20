{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.OrphanInstances.PayoutLifecycleEvent where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent
import qualified Lib.Payment.Storage.Beam.PayoutLifecycleEvent as Beam

instance FromTType' Beam.PayoutLifecycleEvent Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent where
  fromTType' (Beam.PayoutLifecycleEventT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent
          { amount = amount,
            batchId = Kernel.Types.Id.Id batchId,
            batchNo = batchNo,
            createdAt = createdAt,
            currency = currency,
            driverId = driverId,
            eventType = eventType,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            partnerCode = partnerCode,
            payoutOrderId = Kernel.Types.Id.Id payoutOrderId,
            payoutRequestId = payoutRequestId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutLifecycleEvent Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent where
  toTType' (Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent {..}) = do
    Beam.PayoutLifecycleEventT
      { Beam.amount = amount,
        Beam.batchId = Kernel.Types.Id.getId batchId,
        Beam.batchNo = batchNo,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.driverId = driverId,
        Beam.eventType = eventType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.partnerCode = partnerCode,
        Beam.payoutOrderId = Kernel.Types.Id.getId payoutOrderId,
        Beam.payoutRequestId = payoutRequestId,
        Beam.updatedAt = updatedAt
      }
