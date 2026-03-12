module Lib.Finance.Storage.Queries.ReconciliationEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as Domain
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
