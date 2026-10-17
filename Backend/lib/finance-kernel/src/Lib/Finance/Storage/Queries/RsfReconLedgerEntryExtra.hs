{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfReconLedgerEntryExtra
  ( findMessageReceived,
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
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.RsfReconLedgerEntry as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.RsfReconLedgerEntry ()
import qualified Sequelize as Se

-- Phase 1 (E3): a message_id is used once per merchant.
findMessageReceived ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe Domain.RsfReconLedgerEntry)
findMessageReceived merchantId messageId =
  listToMaybe
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.messageId $ Se.Eq (Just messageId),
            Se.Is Beam.entryType $ Se.Eq Domain.MESSAGE_RECEIVED
          ]
      ]

findAllByMerchantAndMessageId ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m [Domain.RsfReconLedgerEntry]
findAllByMerchantAndMessageId merchantId messageId =
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
  findAllWithKV
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
