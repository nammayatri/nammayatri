module Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra
  ( findByReferenceIds,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as Beam
import Lib.Finance.Storage.Queries.IndirectTaxTransaction ()
import qualified Sequelize as Se

-- | Bulk shape used by the reconciliation framework: fetch every indirect
--   tax transaction whose reference_id is in the given set. Replaces
--   per-id loops in the recipes' fetchers.
findByReferenceIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.IndirectTaxTransaction]
findByReferenceIds [] = pure []
findByReferenceIds referenceIds =
  findAllWithKV [Se.Is Beam.referenceId $ Se.In referenceIds]
