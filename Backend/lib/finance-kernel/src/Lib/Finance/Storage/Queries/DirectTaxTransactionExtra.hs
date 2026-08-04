module Lib.Finance.Storage.Queries.DirectTaxTransactionExtra
  ( findByReferenceIds,
    findDeductedByDateRange,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.DirectTaxTransaction ()
import qualified Sequelize as Se

-- | Bulk shape used by the reconciliation framework: fetch every direct
--   tax transaction whose reference_id is in the given set. Replaces
--   per-id loops in the recipes' fetchers.
findByReferenceIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.DirectTaxTransaction]
findByReferenceIds [] = pure []
findByReferenceIds referenceIds =
  findAllWithKV [Se.Is Beam.referenceId $ Se.In referenceIds]

-- | Row-level twin of SAP TDS deduction aggregate (`findTdsDeductionTotals` in RideRevenueTotals).
-- Same filters: mocId + Deducted + transactionDate range.
-- Uses KV (`findAllWithOptionsKV`); mocId/tdsTreatment are WHERE filters, not secondary keys.
findDeductedByDateRange ::
  (BeamFlow m r) =>
  Text ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.DirectTaxTransaction]
findDeductedByDateRange merchantOperatingCityId startTime endTime mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.tdsTreatment $ Se.Eq Domain.Deducted,
          Se.Is Beam.transactionDate $ Se.GreaterThanOrEq startTime,
          Se.Is Beam.transactionDate $ Se.LessThanOrEq endTime
        ]
    ]
    (Se.Desc Beam.transactionDate)
    (Just $ min 100 $ fromMaybe 20 mbLimit)
    mbOffset
