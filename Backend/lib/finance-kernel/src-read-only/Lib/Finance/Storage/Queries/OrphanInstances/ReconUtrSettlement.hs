{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.ReconUtrSettlement where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import qualified Lib.Finance.Storage.Beam.ReconUtrSettlement as Beam

instance FromTType' Beam.ReconUtrSettlement Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement where
  fromTType' (Beam.ReconUtrSettlementT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement
          { approvedAt = approvedAt,
            approvedBy = approvedBy,
            bankVerifiedAmount = bankVerifiedAmount,
            bapId = bapId,
            bapUri = bapUri,
            claimedTotalAmount = claimedTotalAmount,
            createdAt = createdAt,
            deadline = deadline,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            resolutionStatus = resolutionStatus,
            sendAttempts = sendAttempts,
            sendStatus = sendStatus,
            sentAt = sentAt,
            totalOrders = totalOrders,
            updatedAt = updatedAt,
            utr = utr
          }

instance ToTType' Beam.ReconUtrSettlement Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement where
  toTType' (Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement {..}) = do
    Beam.ReconUtrSettlementT
      { Beam.approvedAt = approvedAt,
        Beam.approvedBy = approvedBy,
        Beam.bankVerifiedAmount = bankVerifiedAmount,
        Beam.bapId = bapId,
        Beam.bapUri = bapUri,
        Beam.claimedTotalAmount = claimedTotalAmount,
        Beam.createdAt = createdAt,
        Beam.deadline = deadline,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.resolutionStatus = resolutionStatus,
        Beam.sendAttempts = sendAttempts,
        Beam.sendStatus = sendStatus,
        Beam.sentAt = sentAt,
        Beam.totalOrders = totalOrders,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr
      }
