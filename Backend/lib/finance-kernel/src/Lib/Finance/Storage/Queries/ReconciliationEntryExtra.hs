module Lib.Finance.Storage.Queries.ReconciliationEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as Domain
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as DomainSummary
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconciliationEntry as Beam
import Lib.Finance.Storage.Queries.ReconciliationEntry ()
import qualified Sequelize as Se

findBySourceIdsAndType ::
  (BeamFlow m r) =>
  [Text] ->
  Domain.ReconciliationType ->
  m [Domain.ReconciliationEntry]
findBySourceIdsAndType sourceIds reconciliationType
  | null sourceIds = pure []
  | otherwise =
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.sourceId $ Se.In (map Just sourceIds),
            Se.Is Beam.reconciliationType $ Se.Eq reconciliationType
          ]
      ]

findBySummaryIdWithPagination ::
  (BeamFlow m r) =>
  Id DomainSummary.ReconciliationSummary ->
  Int ->
  Int ->
  m [Domain.ReconciliationEntry]
findBySummaryIdWithPagination summaryId limit offset =
  findAllWithOptionsKV
    [Se.Is Beam.summaryId $ Se.Eq (getId summaryId)]
    (Se.Asc Beam.createdAt)
    (Just limit)
    (Just offset)

findBySettlementId ::
  (BeamFlow m r) =>
  Text -> -- settlementId
  m [Domain.ReconciliationEntry]
findBySettlementId settlementId =
  findAllWithKV
    [Se.Is Beam.settlementId $ Se.Eq (Just settlementId)]
