{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconUtrSettlementExtra
  ( upsertByUtr,
  )
where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconUtrSettlement as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.ReconUtrSettlement ()
import qualified Sequelize as Se

upsertByUtr ::
  (BeamFlow m r) =>
  Domain.ReconUtrSettlement ->
  m ()
upsertByUtr settlement = do
  existing <- findOneWithKV [Se.Is Beam.utr $ Se.Eq settlement.utr]
  case (existing :: Maybe Domain.ReconUtrSettlement) of
    Nothing -> createWithKV settlement
    Just _ -> do
      now <- getCurrentTime
      updateWithKV
        [ Se.Set Beam.claimedTotalAmount settlement.claimedTotalAmount,
          Se.Set Beam.totalOrders settlement.totalOrders,
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.utr $ Se.Eq settlement.utr]
