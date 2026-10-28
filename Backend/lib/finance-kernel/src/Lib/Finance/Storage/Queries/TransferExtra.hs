module Lib.Finance.Storage.Queries.TransferExtra where

import qualified Data.Aeson
import qualified Data.Text as T
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
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

-- | The rail call returned after a webhook or poll may already have moved the row on: record only
--   what the call alone knows (the rail's order id, the attempt count) and leave the status, the raw
--   rail status and the error exactly as the later writer left them.
stampSubmissionById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Maybe Text -> -- rail order id
  Int -> -- attempt count
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
stampSubmissionById mbRailOrderId attemptCount transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.attemptCount attemptCount, Se.Set Beam.updatedAt now]
        <> maybe [] (\oid -> [Se.Set Beam.railOrderId (Just oid)]) mbRailOrderId
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId)]

-- | 'updateSubmissionById' that only applies while the row is CREATED, SUBMITTED or IN_FLIGHT (not
--   parked or settled), so the result of the rail call can never overwrite a status a webhook or
--   poll has already written.
updateSubmissionWhileCreatedById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferStatus ->
  Maybe Text ->
  Maybe Text ->
  Int ->
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateSubmissionWhileCreatedById status mbRailOrderId mbRailStatus attemptCount transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set Beam.status status,
        Se.Set Beam.attemptCount attemptCount,
        Se.Set Beam.updatedAt now
      ]
        <> maybe [] (\oid -> [Se.Set Beam.railOrderId (Just oid)]) mbRailOrderId
        <> maybe [] (\rs -> [Se.Set Beam.railStatus (Just rs)]) mbRailStatus
    )
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId), Se.Is Beam.status $ Se.In [Domain.CREATED, Domain.SUBMITTED, Domain.IN_FLIGHT]]]

-- | 'updateStatusById' that only applies while the row has not been parked or settled.
updateStatusWhileCreatedById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferStatus ->
  Maybe Text ->
  Maybe Text ->
  Kernel.Types.Id.Id Domain.Transfer ->
  m ()
updateStatusWhileCreatedById status mbRailStatus mbLastError transferId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.status status, Se.Set Beam.updatedAt now]
        <> maybe [] (\rs -> [Se.Set Beam.railStatus (Just rs)]) mbRailStatus
        <> maybe [] (\le -> [Se.Set Beam.lastError (Just le)]) mbLastError
    )
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId transferId), Se.Is Beam.status $ Se.In [Domain.CREATED, Domain.SUBMITTED, Domain.IN_FLIGHT]]]

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

-- | Open (non-terminal) INBOUND intents for one account + reference type — the topup
--   reuse scan. Status/direction are not KV keys so this reads through to the DB, but
--   the (accountId) index + tight predicates keep it a few rows instead of the
--   account's full transfer history.
findOpenInboundByAccountAndRefType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Text ->
  m [Domain.Transfer]
findOpenInboundByAccountAndRefType accountId referenceType =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.accountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.direction $ Se.Eq Domain.INBOUND,
          Se.Is Beam.referenceType $ Se.Eq referenceType,
          Se.Is Beam.status $ Se.In [Domain.CREATED, Domain.SUBMITTED, Domain.IN_FLIGHT]
        ]
    ]

-- | OUTBOUND transfers for one account created at/after an instant — the per-day cap
--   count. Time-bounded at the DB, never the full history.
findOutboundByAccountSince ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Kernel.Prelude.UTCTime ->
  m [Domain.Transfer]
findOutboundByAccountSince accountId since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.accountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.direction $ Se.Eq Domain.OUTBOUND,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq since
        ]
    ]

-- | Total of the account's OUTBOUND transfers whose money the balance does not show yet:
--   CREATED with no hold posted (a rail call about to move it; only rows created at/after
--   the given instant), and NEEDS_REVIEW rows parked because the hold failed after an
--   accepted submit (the partner may hold the money; no time limit, ops resolve them).
--   Other NEEDS_REVIEW rows carry no wallet outflow of their own and are ignored.
sumUnheldOutboundSince ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Kernel.Prelude.UTCTime ->
  m HighPrecMoney
sumUnheldOutboundSince accountId since = do
  rows <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.accountId $ Se.Eq (Kernel.Types.Id.getId accountId),
            Se.Is Beam.direction $ Se.Eq Domain.OUTBOUND,
            Se.Or
              [ Se.And [Se.Is Beam.status $ Se.Eq Domain.CREATED, Se.Is Beam.createdAt $ Se.GreaterThanOrEq since],
                Se.Is Beam.status $ Se.Eq Domain.NEEDS_REVIEW
              ]
          ]
      ]
  pure $ sum [t.amount | t <- rows, isNothing t.holdEntryId, t.status == Domain.CREATED || holdFailedAfterSubmit t]
  where
    holdFailedAfterSubmit t = "HOLD_FAILED_AFTER_SUBMIT" `T.isPrefixOf` fromMaybe "" t.lastError

-- | One page of an account's transfers, newest first, optionally one direction.
--   Sorting and paging happen at the DB (accountId index) — never load an account's
--   full transfer history to page it in memory.
findByAccountWithOptions ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Maybe Domain.TransferDirection ->
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [Domain.Transfer]
findByAccountWithOptions accountId mbDirection limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.accountId $ Se.Eq (Kernel.Types.Id.getId accountId)]
            <> [Se.Is Beam.direction $ Se.Eq d | Just d <- [mbDirection]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Transfers for a batch of domain reference ids of one type/direction — bulk status
--   resolution for a page of history rows (referenceId is a KV secondary key).
findByDirectionRefTypeAndRefIds ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Domain.TransferDirection ->
  Text ->
  [Text] ->
  m [Domain.Transfer]
findByDirectionRefTypeAndRefIds _ _ [] = pure []
findByDirectionRefTypeAndRefIds direction referenceType referenceIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.direction $ Se.Eq direction,
          Se.Is Beam.referenceType $ Se.Eq referenceType,
          Se.Is Beam.referenceId $ Se.In referenceIds
        ]
    ]

-- | Ops view: non-terminal transfers for a merchant (NEEDS_REVIEW queue, stuck
--   IN_FLIGHT audits). Status is not a KV key, so this reads through to the DB —
--   acceptable for dashboard/job cadence, not for hot paths.
findAllByMerchantAndStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text -> -- merchant id
  Maybe Text -> -- merchant operating city id
  [Domain.TransferStatus] ->
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [Domain.Transfer]
findAllByMerchantAndStatus merchantId mbCityId statuses limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.status $ Se.In statuses
          ]
            <> [Se.Is Beam.merchantOperatingCityId $ Se.Eq cityId | Just cityId <- [mbCityId]]
        )
    ]
    (Se.Desc Beam.updatedAt)
    limit
    offset
