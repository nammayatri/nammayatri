{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfReconLedgerEntryExtra
  ( findMessageReceived,
    findMessageProcessed,
    findAllByMerchantAndMessageId,
    findAllByMerchantAndOrderIds,
    findAllByMerchantAndUtrs,
    findAllByMerchantEntryTypesAndUtrs,
    findAllByMerchantEntryTypeAndOrderIds,
    findClaimsByMerchantStatusAndOrderIds,
    findClaimsByMerchantStatusAndUtrs,
    findClaimsByMerchantStatusAndCreatedAtRange,
    findAllByMerchantEntryTypeAndCreatedAtRange,
    updateClaimStatus,
    updateClaimStatusByIds,
  )
where

import Database.Beam.Postgres (Postgres)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, roundHighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.RsfReconLedgerEntry as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.RsfReconLedgerEntry ()
import qualified Sequelize as Se

-- Every ledger read goes through here. A row read back from KV arrives through JSON as a Double,
-- so 425.78 comes back as 425.7799999999999727...; rounding to paise makes it equal again to the
-- exact value it was written with (and to amounts parsed from the wire or summed in SQL), so the
-- equality / <= comparisons in the recon checks and verdicts hold.
findLedgerRows :: (BeamFlow m r) => [Se.Clause Postgres Beam.RsfReconLedgerEntryT] -> m [Domain.RsfReconLedgerEntry]
findLedgerRows conditions = map toPaise <$> findAllWithKV conditions
  where
    toPaise e =
      e
        { Domain.amount = paise e.amount,
          Domain.orderPaymentAmount = paise <$> e.orderPaymentAmount,
          Domain.bffAmount = paise <$> e.bffAmount,
          Domain.withholdingTaxGst = paise <$> e.withholdingTaxGst,
          Domain.withholdingTaxTds = paise <$> e.withholdingTaxTds,
          Domain.deductionByCollector = paise <$> e.deductionByCollector,
          Domain.rejectionDiff = paise <$> e.rejectionDiff
        }
    paise = roundHighPrecMoney @HighPrecMoney 2

-- Phase 1 (E3): a message_id is used once per merchant.
findMessageReceived ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe Domain.RsfReconLedgerEntry)
findMessageReceived merchantId messageId =
  listToMaybe
    <$> findLedgerRows
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.messageId $ Se.Eq (Just messageId),
            Se.Is Beam.entryType $ Se.Eq Domain.MESSAGE_RECEIVED
          ]
      ]

-- Phase 1 (E3): a message is a duplicate only once it was fully processed; a half-processed one is taken
-- again so a retry can finish it.
findMessageProcessed ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe Domain.RsfReconLedgerEntry)
findMessageProcessed merchantId messageId =
  listToMaybe
    <$> findLedgerRows
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.messageId $ Se.Eq (Just messageId),
            Se.Is Beam.entryType $ Se.Eq Domain.MESSAGE_PROCESSED
          ]
      ]

findAllByMerchantAndMessageId ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantAndMessageId merchantId messageId =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.messageId $ Se.Eq (Just messageId)
        ]
    ]

findAllByMerchantAndOrderIds ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantAndOrderIds _ [] = pure []
findAllByMerchantAndOrderIds merchantId orderIds =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.orderId $ Se.In (map Just orderIds)
        ]
    ]

findAllByMerchantAndUtrs ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantAndUtrs _ [] = pure []
findAllByMerchantAndUtrs merchantId utrs =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.utr $ Se.In (map Just utrs)
        ]
    ]

findAllByMerchantEntryTypesAndUtrs ::
  (BeamFlow m r) =>
  Text ->
  [Domain.RsfLedgerEntryType] ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantEntryTypesAndUtrs _ _ [] = pure []
findAllByMerchantEntryTypesAndUtrs merchantId entryTypes utrs =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.In entryTypes,
          Se.Is Beam.utr $ Se.In (map Just utrs)
        ]
    ]

findAllByMerchantEntryTypeAndOrderIds ::
  (BeamFlow m r) =>
  Text ->
  Domain.RsfLedgerEntryType ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantEntryTypeAndOrderIds _ _ [] = pure []
findAllByMerchantEntryTypeAndOrderIds merchantId entryType orderIds =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.Eq entryType,
          Se.Is Beam.orderId $ Se.In (map Just orderIds)
        ]
    ]

-- BAP_CLAIM rows of the given orders in one claim status (stage B reads the PENDING ones).
findClaimsByMerchantStatusAndOrderIds ::
  (BeamFlow m r) =>
  Text ->
  Domain.RsfClaimStatus ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findClaimsByMerchantStatusAndOrderIds _ _ [] = pure []
findClaimsByMerchantStatusAndOrderIds merchantId claimStatus orderIds =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.Eq Domain.BAP_CLAIM,
          Se.Is Beam.claimStatus $ Se.Eq (Just claimStatus),
          Se.Is Beam.orderId $ Se.In (map Just orderIds)
        ]
    ]

findClaimsByMerchantStatusAndUtrs ::
  (BeamFlow m r) =>
  Text ->
  Domain.RsfClaimStatus ->
  [Text] ->
  m [Domain.RsfReconLedgerEntry]
findClaimsByMerchantStatusAndUtrs _ _ [] = pure []
findClaimsByMerchantStatusAndUtrs merchantId claimStatus utrs =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.Eq Domain.BAP_CLAIM,
          Se.Is Beam.claimStatus $ Se.Eq (Just claimStatus),
          Se.Is Beam.utr $ Se.In (map Just utrs)
        ]
    ]

-- BAP_CLAIM rows in one claim status whose receiver_recon arrived in the range (auto-allocation
-- reads the day's ACCEPTED claims this way).
findClaimsByMerchantStatusAndCreatedAtRange ::
  (BeamFlow m r) =>
  Text ->
  Domain.RsfClaimStatus ->
  UTCTime ->
  UTCTime ->
  m [Domain.RsfReconLedgerEntry]
findClaimsByMerchantStatusAndCreatedAtRange merchantId claimStatus from to =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.Eq Domain.BAP_CLAIM,
          Se.Is Beam.claimStatus $ Se.Eq (Just claimStatus),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
        ]
    ]

findAllByMerchantEntryTypeAndCreatedAtRange ::
  (BeamFlow m r) =>
  Text ->
  Domain.RsfLedgerEntryType ->
  UTCTime ->
  UTCTime ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantEntryTypeAndCreatedAtRange merchantId entryType from to =
  findLedgerRows
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entryType $ Se.Eq entryType,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
        ]
    ]

-- Phase 2: the only UPDATE the ledger permits -- stages A/B/C settle a claim's status in place.
updateClaimStatus ::
  (BeamFlow m r) =>
  Id Domain.RsfReconLedgerEntry ->
  Domain.RsfClaimStatus ->
  Maybe HighPrecMoney ->
  m ()
updateClaimStatus entryId claimStatus rejectionDiff = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.claimStatus (Just claimStatus),
      Se.Set Beam.rejectionDiff rejectionDiff,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (getId entryId),
          Se.Is Beam.entryType $ Se.Eq Domain.BAP_CLAIM
        ]
    ]

-- The same, for many claims settled to one status in a single UPDATE.
updateClaimStatusByIds ::
  (BeamFlow m r) =>
  [Id Domain.RsfReconLedgerEntry] ->
  Domain.RsfClaimStatus ->
  Maybe HighPrecMoney ->
  m ()
updateClaimStatusByIds [] _ _ = pure ()
updateClaimStatusByIds entryIds claimStatus rejectionDiff = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.claimStatus (Just claimStatus),
      Se.Set Beam.rejectionDiff rejectionDiff,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.In (map getId entryIds),
          Se.Is Beam.entryType $ Se.Eq Domain.BAP_CLAIM
        ]
    ]
