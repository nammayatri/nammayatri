module Lib.Finance.Storage.Queries.DirectTaxTransactionExtra
  ( findByReferenceIds,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as Beam
import Lib.Finance.Storage.Queries.DirectTaxTransaction ()
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
