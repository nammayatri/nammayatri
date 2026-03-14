module Lib.Finance.Storage.Queries.ReconciliationSummaryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconciliationSummary as Beam
import Lib.Finance.Storage.Queries.ReconciliationSummary ()
import qualified Sequelize as Se

findByDateRangeAndType ::
  (BeamFlow m r) =>
  Maybe UTCTime ->
  Maybe UTCTime ->
  Domain.ReconciliationType ->
  m [Domain.ReconciliationSummary]
findByDateRangeAndType mbFromDate mbToDate reconciliationType =
  findAllWithKV $
    [ Se.Is Beam.reconciliationType $ Se.Eq reconciliationType
    ]
      <> maybe [] (\from -> [Se.Is Beam.reconciliationDate $ Se.GreaterThanOrEq from]) mbFromDate
      <> maybe [] (\to -> [Se.Is Beam.reconciliationDate $ Se.LessThanOrEq to]) mbToDate
