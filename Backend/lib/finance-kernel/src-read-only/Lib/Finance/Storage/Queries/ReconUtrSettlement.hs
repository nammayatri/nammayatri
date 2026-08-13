{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconUtrSettlement (module Lib.Finance.Storage.Queries.ReconUtrSettlement, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconUtrSettlement as Beam
import Lib.Finance.Storage.Queries.ReconUtrSettlementExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement -> m (Maybe Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByUtr :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement))
findByUtr utr = do findOneWithKV [Se.Is Beam.utr $ Se.Eq utr]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement -> m (Maybe Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankVerifiedAmount bankVerifiedAmount,
      Se.Set Beam.bapId bapId,
      Se.Set Beam.bapUri bapUri,
      Se.Set Beam.claimedTotalAmount claimedTotalAmount,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.resolvedBy resolvedBy,
      Se.Set Beam.totalOrders totalOrders,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utr utr
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
