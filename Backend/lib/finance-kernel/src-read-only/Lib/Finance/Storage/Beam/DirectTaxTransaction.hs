{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.DirectTaxTransaction where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction

data DirectTaxTransactionT f = DirectTaxTransactionT
  { counterpartyId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    grossAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    netAmountPaid :: (B.C f Kernel.Types.Common.HighPrecMoney),
    panOfParty :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    paymentDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    referenceId :: (B.C f Kernel.Prelude.Text),
    tanOfDeductee :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    tdsAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tdsRate :: (B.C f Kernel.Prelude.Double),
    tdsTreatment :: (B.C f Lib.Finance.Domain.Types.DirectTaxTransaction.TdsTreatment),
    transactionDate :: (B.C f Kernel.Prelude.UTCTime),
    transactionType :: (B.C f Lib.Finance.Domain.Types.DirectTaxTransaction.TransactionType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DirectTaxTransactionT where
  data PrimaryKey DirectTaxTransactionT f = DirectTaxTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DirectTaxTransactionId . id

type DirectTaxTransaction = DirectTaxTransactionT Identity

$(enableKVPG (''DirectTaxTransactionT) [('id)] [[('referenceId)]])

$(mkTableInstancesGenericSchema (''DirectTaxTransactionT) "direct_tax_transaction")
