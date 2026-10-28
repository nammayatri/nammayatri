{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RsfOrderState where

import qualified Domain.Types.RsfOrderState
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RsfOrderState as Beam

instance FromTType' Beam.RsfOrderState Domain.Types.RsfOrderState.RsfOrderState where
  fromTType' (Beam.RsfOrderStateT {..}) = do
    pure $
      Just
        Domain.Types.RsfOrderState.RsfOrderState
          { bapId = bapId,
            createdAt = createdAt,
            lastClaimedAt = lastClaimedAt,
            lastReportedAt = lastReportedAt,
            lastReportedMessageId = lastReportedMessageId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            orderId = orderId,
            reportedCode = reportedCode,
            reportedDiff = reportedDiff,
            reportedStatus = reportedStatus,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RsfOrderState Domain.Types.RsfOrderState.RsfOrderState where
  toTType' (Domain.Types.RsfOrderState.RsfOrderState {..}) = do
    Beam.RsfOrderStateT
      { Beam.bapId = bapId,
        Beam.createdAt = createdAt,
        Beam.lastClaimedAt = lastClaimedAt,
        Beam.lastReportedAt = lastReportedAt,
        Beam.lastReportedMessageId = lastReportedMessageId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.orderId = orderId,
        Beam.reportedCode = reportedCode,
        Beam.reportedDiff = reportedDiff,
        Beam.reportedStatus = reportedStatus,
        Beam.updatedAt = updatedAt
      }
