{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.RsfOrderState where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfOrderState
import qualified Lib.Finance.Storage.Beam.RsfOrderState as Beam

instance FromTType' Beam.RsfOrderState Lib.Finance.Domain.Types.RsfOrderState.RsfOrderState where
  fromTType' (Beam.RsfOrderStateT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.RsfOrderState.RsfOrderState
          { createdAt = createdAt,
            lastReportedAt = lastReportedAt,
            lastReportedMessageId = lastReportedMessageId,
            merchantId = merchantId,
            orderId = orderId,
            reportedCode = reportedCode,
            reportedDiff = reportedDiff,
            reportedStatus = reportedStatus,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RsfOrderState Lib.Finance.Domain.Types.RsfOrderState.RsfOrderState where
  toTType' (Lib.Finance.Domain.Types.RsfOrderState.RsfOrderState {..}) = do
    Beam.RsfOrderStateT
      { Beam.createdAt = createdAt,
        Beam.lastReportedAt = lastReportedAt,
        Beam.lastReportedMessageId = lastReportedMessageId,
        Beam.merchantId = merchantId,
        Beam.orderId = orderId,
        Beam.reportedCode = reportedCode,
        Beam.reportedDiff = reportedDiff,
        Beam.reportedStatus = reportedStatus,
        Beam.updatedAt = updatedAt
      }
