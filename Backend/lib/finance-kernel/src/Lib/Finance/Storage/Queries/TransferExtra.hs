module Lib.Finance.Storage.Queries.TransferExtra where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.Transfer as Domain
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.Transfer as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.Transfer ()
import qualified Sequelize as Se

-- | Status move, optionally stamping the latest raw rail status / error. Fields not
--   provided are left untouched (a PENDING poll must not erase a stored error).
updateStatusById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferStatus ->
  Maybe Text -> -- raw rail status, verbatim
  Maybe Text -> -- last error
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateStatusById status mbRailStatus mbLastError transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.status status, Se.Set Beam.updatedAt now]
        <> maybe [] (\rs -> [Se.Set Beam.railStatus (Just rs)]) mbRailStatus
        <> maybe [] (\le -> [Se.Set Beam.lastError (Just le)]) mbLastError
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

-- | Drop a stale error once the transfer has settled: a late success after a failure
--   (or NEEDS_REVIEW) must not keep reading as an error on the row.
clearLastErrorById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
clearLastErrorById transferId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.lastError Nothing, Se.Set Beam.updatedAt now]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

-- | Record the result of handing the transfer to a rail.
updateSubmissionById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferStatus ->
  Maybe Text -> -- rail order id
  Maybe Text -> -- raw rail status
  Int -> -- new attempt count
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateSubmissionById status mbRailOrderId mbRailStatus attemptCount transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set Beam.status status,
        Se.Set Beam.attemptCount attemptCount,
        Se.Set Beam.updatedAt now
      ]
        <> maybe [] (\oid -> [Se.Set Beam.railOrderId (Just oid)]) mbRailOrderId
        <> maybe [] (\rs -> [Se.Set Beam.railStatus (Just rs)]) mbRailStatus
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

-- | Attach the ledger legs this transfer has posted so far.
updateLedgerLegsById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Maybe Text -> -- hold entry id
  [Text] -> -- all entry ids (caller passes the full, merged list)
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateLedgerLegsById mbHoldEntryId entryIds transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.entryIds (Just entryIds), Se.Set Beam.updatedAt now]
        <> maybe [] (\h -> [Se.Set Beam.holdEntryId (Just h)]) mbHoldEntryId
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

updateMetadataById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferMetadata ->
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateMetadataById metadata transferId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.metadata (Just (Data.Aeson.toJSON metadata)), Se.Set Beam.updatedAt now]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

-- | Ops view: non-terminal transfers for a merchant (NEEDS_REVIEW queue, stuck
--   IN_FLIGHT audits). Status is not a KV key, so this reads through to the DB —
--   acceptable for dashboard/job cadence, not for hot paths.
findAllByMerchantAndStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  [Domain.TransferStatus] ->
  m [Domain.Transfer]
findAllByMerchantAndStatus merchantId statuses =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.status $ Se.In statuses
        ]
    ]
