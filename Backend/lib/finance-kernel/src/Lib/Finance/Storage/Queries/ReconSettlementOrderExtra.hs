{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconSettlementOrderExtra
  ( messageIdExists,
  )
where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconSettlementOrder as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.ReconSettlementOrder ()
import qualified Sequelize as Se

messageIdExists ::
  (BeamFlow m r) =>
  Text ->
  m Bool
messageIdExists msgId = do
  rows <- findAllWithKV [Se.Is Beam.messageId $ Se.Eq msgId]
  pure $ not (null (rows :: [Domain.ReconSettlementOrder]))
