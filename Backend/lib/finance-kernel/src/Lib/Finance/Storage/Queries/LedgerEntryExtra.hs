{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.LedgerEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Domain.Types.LedgerEntry as Domain
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.LedgerEntry as Beam
import Lib.Finance.Storage.Queries.LedgerEntry ()
import qualified Sequelize as Se

findByIds ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  [Kernel.Types.Id.Id Domain.LedgerEntry] ->
  m [Domain.LedgerEntry]
findByIds [] = pure []
findByIds entryIds =
  findAllWithKV [Se.Is Beam.id $ Se.In (map Kernel.Types.Id.getId entryIds)]

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
