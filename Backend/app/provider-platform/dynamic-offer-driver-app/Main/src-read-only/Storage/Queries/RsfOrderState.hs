{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RsfOrderState (module Storage.Queries.RsfOrderState, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.RsfOrderState
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RsfOrderState as Beam
import Storage.Queries.RsfOrderStateExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RsfOrderState.RsfOrderState -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RsfOrderState.RsfOrderState] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RsfOrderState.RsfOrderState))
findByPrimaryKey merchantId orderId = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.orderId $ Se.Eq orderId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RsfOrderState.RsfOrderState -> m ())
updateByPrimaryKey (Domain.Types.RsfOrderState.RsfOrderState {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapId bapId,
      Se.Set Beam.lastClaimedAt lastClaimedAt,
      Se.Set Beam.lastReportedAt lastReportedAt,
      Se.Set Beam.lastReportedMessageId lastReportedMessageId,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reportedCode reportedCode,
      Se.Set Beam.reportedDiff reportedDiff,
      Se.Set Beam.reportedStatus reportedStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.orderId $ Se.Eq orderId]]
