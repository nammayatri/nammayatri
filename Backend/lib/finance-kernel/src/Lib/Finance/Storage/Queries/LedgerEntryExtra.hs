module Lib.Finance.Storage.Queries.LedgerEntryExtra where

import Data.List (nub)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Domain.Types.LedgerEntry as Domain
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.LedgerEntry as Beam
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import Lib.Finance.Storage.Queries.LedgerEntry ()
import qualified Sequelize as Se

findByIds ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Kernel.Types.Id.Id Domain.LedgerEntry] ->
  m [Domain.LedgerEntry]
findByIds [] = pure []
findByIds entryIds =
  findAllWithKV [Se.Is Beam.id $ Se.In (map Kernel.Types.Id.getId entryIds)]

-- | Live ledger holds against an account: PENDING entries debiting it, EXCLUDING legs
--   whose counterpart is a GOVERNMENT account. A pending tax accrual (e.g. TDS on a
--   not-yet-collected cancellation fee) deducts from a pending INFLOW by construction
--   — never from the existing balance — so counting it as a hold double-penalizes.
--   Real holds (statutory, booking-deposit, prepaid) all point at platform-side
--   accounts and sum unchanged.
sumPendingByFromAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  m HighPrecMoney
sumPendingByFromAccount accountId = do
  entries <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
            Se.Is Beam.status $ Se.Eq Domain.PENDING
          ]
      ]
  let toAccountIds = nub (map (.toAccountId) entries)
  govIds <- flip filterM toAccountIds $ \toAccId -> do
    mbTo <- QAccount.findById toAccId
    pure $ case mbTo >>= (.counterpartyType) of
      Just Lib.Finance.Domain.Types.Account.GOVERNMENT_DIRECT -> True
      Just Lib.Finance.Domain.Types.Account.GOVERNMENT_INDIRECT -> True
      _ -> False
  pure $ sum [e.amount | e <- entries, e.toAccountId `notElem` govIds]

-- | Every ledger entry under one reference id, whatever its reference type.
findByReferenceId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  m [Domain.LedgerEntry]
findByReferenceId referenceId =
  findAllWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]

-- | Find ledger entries by reference type (IN list) and reference ID
findByReferenceIn ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Text] ->
  Text ->
  m ([Domain.LedgerEntry])
findByReferenceIn referenceTypes referenceId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.referenceType $ Se.In referenceTypes,
          Se.Is Beam.referenceId $ Se.Eq referenceId
        ]
    ]

-- | Bulk shape used by the reconciliation framework: many reference types AND
--   many reference ids. Single indexed query — replaces per-id
--   'findByReferenceIn' loops in every ledger-touching recipe.
findByReferenceTypesAndReferenceIds ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Text] -> -- reference types
  [Text] -> -- reference ids
  m [Domain.LedgerEntry]
findByReferenceTypesAndReferenceIds _ [] = pure []
findByReferenceTypesAndReferenceIds [] _ = pure []
findByReferenceTypesAndReferenceIds referenceTypes referenceIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.referenceType $ Se.In referenceTypes,
          Se.Is Beam.referenceId $ Se.In referenceIds
        ]
    ]

findByReferenceTypesAndDateRange ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Text] ->
  Text ->
  UTCTime ->
  UTCTime ->
  m [Domain.LedgerEntry]
findByReferenceTypesAndDateRange referenceTypes merchantOperatingCityId startTime endTime =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.referenceType $ Se.In referenceTypes,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.timestamp $ Se.GreaterThanOrEq startTime,
          Se.Is Beam.timestamp $ Se.LessThanOrEq endTime
        ]
    ]

findSettledByReferenceTypeAndDateRange ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [Domain.LedgerEntry]
findSettledByReferenceTypeAndDateRange referenceType merchantOperatingCityId startTime endTime limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.referenceType $ Se.Eq referenceType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.status $ Se.Eq Domain.SETTLED,
          Se.Is Beam.timestamp $ Se.GreaterThanOrEq startTime,
          Se.Is Beam.timestamp $ Se.LessThanOrEq endTime,
          Se.Is Beam.reversalOf $ Se.Eq Nothing
        ]
    ]
    (Se.Desc Beam.timestamp)
    limit
    offset

-- | Find ALL ledger entries by referenceId (no referenceType filter)
findAllByReferenceId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  m ([Domain.LedgerEntry])
findAllByReferenceId refId =
  findAllWithKV [Se.Is Beam.referenceId $ Se.Eq refId]

findLatestByAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  m (Maybe Domain.LedgerEntry)
findLatestByAccount accountId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
              Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId accountId)
            ]
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 1)
    (Just 0)
    <&> listToMaybe

findByAccountsWithOptions ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account] ->
  [Text] -> -- referenceTypes filter (empty list = no filter)
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [Domain.LedgerEntry]
findByAccountsWithOptions accountIds referenceTypes mbFrom mbTo limit offset =
  findByAccountsWithConcernedIndividual accountIds referenceTypes mbFrom mbTo limit offset Nothing

-- | Variant of 'findByAccountsWithOptions' that also filters by
--   'concernedIndividualId' when supplied. Used by fleet-driver views where
--   the wallet account is owned by the fleet but each driver should only see
--   their own activity.
findByAccountsWithConcernedIndividual ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account] ->
  [Text] -> -- referenceTypes filter (empty list = no filter)
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Maybe Text -> -- concernedIndividualId
  m [Domain.LedgerEntry]
findByAccountsWithConcernedIndividual accountIds referenceTypes mbFrom mbTo limit offset mbConcernedIndividualId = do
  let accountIdTexts = map Kernel.Types.Id.getId accountIds
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Or
              [ Se.Is Beam.toAccountId $ Se.In accountIdTexts,
                Se.Is Beam.fromAccountId $ Se.In accountIdTexts
              ]
          ]
            <> [Se.Is Beam.referenceType $ Se.In referenceTypes | not (null referenceTypes)]
            <> [Se.Is Beam.timestamp $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.timestamp $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.concernedIndividualId $ Se.Eq mbConcernedIndividualId | isJust mbConcernedIndividualId]
        )
    ]
    (Se.Desc Beam.timestamp)
    limit
    offset

findOpenLotsByToAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  m [Domain.LedgerEntry]
findOpenLotsByToAccount accountId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.entryType $ Se.Eq Domain.LiabilityCreated,
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversedBy $ Se.Eq Nothing,
          Se.Is Beam.reversalOf $ Se.Eq Nothing,
          Se.Or
            [ Se.Is Beam.settlementStatus $ Se.Eq Nothing,
              Se.Is Beam.settlementStatus $ Se.Eq (Just Domain.UNSETTLED)
            ]
        ]
    ]

-- | Entries that spent or expired part of the given lots (entityReferenceType = ExpiringLot, entityReferenceId =
--   the lot's id), skipping voided and reversed ones so a reversed spend frees its amount again.
findLotConsumptions ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Text] ->
  m [Domain.LedgerEntry]
findLotConsumptions [] = pure []
findLotConsumptions lotIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityReferenceType $ Se.Eq (Just Domain.ExpiringLot),
          Se.Is Beam.entityReferenceId $ Se.In (map Just lotIds),
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversedBy $ Se.Eq Nothing
        ]
    ]

-- | Debits out of an account that name no lot: LiabilitySettled entries with no entity reference, not voided or
--   reversed, not yet settled. A debit that lapses is restored and then settled by its owner, so the set stays
--   small; debits that never lapse stay in it.
findUnlinkedDebitsByFromAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  m [Domain.LedgerEntry]
findUnlinkedDebitsByFromAccount accountId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.entryType $ Se.Eq Domain.LiabilitySettled,
          Se.Is Beam.entityReferenceType $ Se.Eq Nothing,
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversedBy $ Se.Eq Nothing,
          Se.Or
            [ Se.Is Beam.settlementStatus $ Se.Eq Nothing,
              Se.Is Beam.settlementStatus $ Se.Eq (Just Domain.UNSETTLED)
            ]
        ]
    ]

-- | Lot credits to an account under one reference type and entity reference (the event that earned them).
findLotsByToAccountReferenceAndEntity ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Text ->
  Maybe Text ->
  m [Domain.LedgerEntry]
findLotsByToAccountReferenceAndEntity accountId referenceType entityReferenceId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.entryType $ Se.Eq Domain.LiabilityCreated,
          Se.Is Beam.referenceType $ Se.Eq referenceType,
          Se.Is Beam.entityReferenceId $ Se.Eq entityReferenceId,
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversalOf $ Se.Eq Nothing
        ]
    ]

-- | An account's own movements in time order, newest first: lot credits into it and debits out of it that name
--   no lot (spending and expiry entries, which name a lot, are left out). From is inclusive, to exclusive.
findAccountActivity ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.LedgerEntry]
findAccountActivity accountId mbFrom mbTo mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Or
            [ Se.And
                [ Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
                  Se.Is Beam.entryType $ Se.Eq Domain.LiabilityCreated
                ],
              Se.And
                [ Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
                  Se.Is Beam.entryType $ Se.Eq Domain.LiabilitySettled,
                  Se.Is Beam.entityReferenceType $ Se.Eq Nothing
                ]
            ]
        ]
          <> [Se.Is Beam.timestamp $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.timestamp $ Se.LessThan to | Just to <- [mbTo]]
    ]
    (Se.Desc Beam.timestamp)
    mbLimit
    mbOffset

-- | 'findOpenLotsByToAccount' narrowed to the lots that lapsed before the given instant.
findLapsedOpenLotsByToAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  UTCTime ->
  m [Domain.LedgerEntry]
findLapsedOpenLotsByToAccount accountId now =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.entryType $ Se.Eq Domain.LiabilityCreated,
          -- the not-null check is not redundant: rows still held in Redis are matched in memory, where
          -- Nothing < Just now is True, so a lot that never expires would count as lapsed
          Se.Is Beam.expiresAt $ Se.Not (Se.Eq Nothing),
          Se.Is Beam.expiresAt $ Se.LessThan (Just now),
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversedBy $ Se.Eq Nothing,
          Se.Is Beam.reversalOf $ Se.Eq Nothing,
          Se.Or
            [ Se.Is Beam.settlementStatus $ Se.Eq Nothing,
              Se.Is Beam.settlementStatus $ Se.Eq (Just Domain.UNSETTLED)
            ]
        ]
    ]

-- | 'findUnlinkedDebitsByFromAccount' narrowed to the debits that lapsed before the given instant.
findLapsedUnlinkedDebitsByFromAccount ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account ->
  UTCTime ->
  m [Domain.LedgerEntry]
findLapsedUnlinkedDebitsByFromAccount accountId now =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId accountId),
          Se.Is Beam.entryType $ Se.Eq Domain.LiabilitySettled,
          Se.Is Beam.entityReferenceType $ Se.Eq Nothing,
          -- see findLapsedOpenLotsByToAccount: keeps a debit with no expiry out of the in-memory match
          Se.Is Beam.expiresAt $ Se.Not (Se.Eq Nothing),
          Se.Is Beam.expiresAt $ Se.LessThan (Just now),
          Se.Is Beam.status $ Se.Not $ Se.Eq Domain.VOIDED,
          Se.Is Beam.reversedBy $ Se.Eq Nothing,
          Se.Or
            [ Se.Is Beam.settlementStatus $ Se.Eq Nothing,
              Se.Is Beam.settlementStatus $ Se.Eq (Just Domain.UNSETTLED)
            ]
        ]
    ]
