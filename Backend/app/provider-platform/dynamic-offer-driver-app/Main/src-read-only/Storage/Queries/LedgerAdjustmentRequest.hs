{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LedgerAdjustmentRequest (module Storage.Queries.LedgerAdjustmentRequest, module ReExport) where

import qualified Domain.Types.LedgerAdjustmentRequest
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Sequelize as Se
import qualified Storage.Beam.LedgerAdjustmentRequest as Beam
import Storage.Queries.LedgerAdjustmentRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest -> m ())
create = createWithKV

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest -> m (Maybe Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByReferenceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest))
findByReferenceId referenceId = do findOneWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]

findByReferenceIdAndStatuses ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Domain.Types.LedgerAdjustmentRequest.AdjustmentRequestStatus] -> m (Maybe Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest))
findByReferenceIdAndStatuses referenceId status = do findOneWithKV [Se.And [Se.Is Beam.referenceId $ Se.Eq referenceId, Se.Is Beam.status $ Se.In status]]

updateStatusAndChecker ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.LedgerAdjustmentRequest.AdjustmentRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest -> m ())
updateStatusAndChecker status adminCheckerId adminCheckerName errorMessage id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.adminCheckerId (Kernel.Types.Id.getId <$> adminCheckerId),
      Se.Set Beam.adminCheckerName adminCheckerName,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusCheckerAndPostResult ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.LedgerAdjustmentRequest.AdjustmentRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest -> m ())
updateStatusCheckerAndPostResult status adminCheckerId adminCheckerName errorMessage ledgerEntryId postedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.adminCheckerId (Kernel.Types.Id.getId <$> adminCheckerId),
      Se.Set Beam.adminCheckerName adminCheckerName,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.ledgerEntryId (Kernel.Types.Id.getId <$> ledgerEntryId),
      Se.Set Beam.postedAt postedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest -> m (Maybe Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
