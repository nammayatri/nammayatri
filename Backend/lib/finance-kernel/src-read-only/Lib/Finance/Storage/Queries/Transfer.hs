{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.Transfer (module Lib.Finance.Storage.Queries.Transfer, module ReExport) where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Domain.Types.Transfer
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.Transfer as Beam
import Lib.Finance.Storage.Queries.TransferExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Transfer.Transfer -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.Transfer.Transfer] -> m ())
createMany = traverse_ create

findAllByAccountId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m ([Lib.Finance.Domain.Types.Transfer.Transfer]))
findAllByAccountId accountId = do findAllWithKV [Se.Is Beam.accountId $ Se.Eq (Kernel.Types.Id.getId accountId)]

findById :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Transfer.Transfer -> m (Maybe Lib.Finance.Domain.Types.Transfer.Transfer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRailOrderId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.Transfer.Transfer))
findByRailOrderId railOrderId = do findOneWithKV [Se.Is Beam.railOrderId $ Se.Eq railOrderId]

findByReference :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.Transfer.Transfer))
findByReference referenceType referenceId = do findOneWithKV [Se.And [Se.Is Beam.referenceType $ Se.Eq referenceType, Se.Is Beam.referenceId $ Se.Eq referenceId]]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Transfer.Transfer -> m (Maybe Lib.Finance.Domain.Types.Transfer.Transfer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Transfer.Transfer -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.Transfer.Transfer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountId (Kernel.Types.Id.getId accountId),
      Se.Set Beam.amount amount,
      Se.Set Beam.attemptCount attemptCount,
      Se.Set Beam.counterAccountId (Kernel.Types.Id.getId <$> counterAccountId),
      Se.Set Beam.coveredEntryIds coveredEntryIds,
      Se.Set Beam.currency currency,
      Se.Set Beam.direction direction,
      Se.Set Beam.entryIds entryIds,
      Se.Set Beam.feeAmount feeAmount,
      Se.Set Beam.holdEntryId (Kernel.Types.Id.getId <$> holdEntryId),
      Se.Set Beam.lastError lastError,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.metadata (Data.Aeson.toJSON <$> metadata),
      Se.Set Beam.railKey railKey,
      Se.Set Beam.railOrderId railOrderId,
      Se.Set Beam.railStatus railStatus,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.referenceType referenceType,
      Se.Set Beam.requestedAmount requestedAmount,
      Se.Set Beam.scheduledFor scheduledFor,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
