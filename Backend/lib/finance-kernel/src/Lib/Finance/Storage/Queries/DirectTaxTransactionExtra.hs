{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.DirectTaxTransactionExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as Beam
-- Import the orphan instances from the main query module
import Lib.Finance.Storage.Queries.DirectTaxTransaction ()
import qualified Sequelize as Se

findByReferenceId :: (BeamFlow m r) => Text -> m [Domain.DirectTaxTransaction]
findByReferenceId referenceId = findAllWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]

findByInvoiceNumber :: (BeamFlow m r) => Text -> m [Domain.DirectTaxTransaction]
findByInvoiceNumber invoiceNumber = findAllWithKV [Se.Is Beam.invoiceNumber $ Se.Eq (Just invoiceNumber)]
