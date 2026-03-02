{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as Beam
-- Import the orphan instances from the main query module
import Lib.Finance.Storage.Queries.IndirectTaxTransaction ()
import qualified Sequelize as Se

findByInvoiceNumber :: (BeamFlow m r) => Text -> m [Domain.IndirectTaxTransaction]
findByInvoiceNumber invoiceNumber = do findAllWithKV [Se.Is Beam.invoiceNumber $ Se.Eq (Just invoiceNumber)]

findByReferenceId :: (BeamFlow m r) => Text -> m [Domain.IndirectTaxTransaction]
findByReferenceId referenceId = findAllWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]
