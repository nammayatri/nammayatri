module Lib.Finance.Storage.Queries.ReconciliationSummaryExtra
  ( findByDateRangeAndSpec,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as Domain
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconciliationSummary as Beam
import Lib.Finance.Storage.Queries.ReconciliationSummary ()
import qualified Sequelize as Se

findByDateRangeAndSpec ::
  (BeamFlow m r) =>
  ReconT.Domain ->
  ReconT.DataSource ->
  ReconT.DataSource ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [Domain.ReconciliationSummary]
findByDateRangeAndSpec domain source target mbFromDate mbToDate =
  findAllWithKV $
    [ Se.Is Beam.domain $ Se.Eq domain,
      Se.Is Beam.source $ Se.Eq source,
      Se.Is Beam.target $ Se.Eq target
    ]
      <> maybe [] (\from -> [Se.Is Beam.reconciliationDate $ Se.GreaterThanOrEq from]) mbFromDate
      <> maybe [] (\to -> [Se.Is Beam.reconciliationDate $ Se.LessThanOrEq to]) mbToDate
