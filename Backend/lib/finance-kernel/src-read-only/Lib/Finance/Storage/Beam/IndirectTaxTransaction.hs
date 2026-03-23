{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.IndirectTaxTransaction where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction
import Tools.Beam.UtilsTH

data IndirectTaxTransactionT f = IndirectTaxTransactionT
  { cgstAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    counterpartyId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    creditOrDebitNoteNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    externalCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    gstCreditType :: B.C f Lib.Finance.Domain.Types.IndirectTaxTransaction.GstCreditType,
    gstRate :: B.C f Kernel.Prelude.Double,
    gstinOfParty :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    igstAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    invoiceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    issuedByTaxNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    issuedToTaxNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    referenceId :: B.C f Kernel.Prelude.Text,
    sacCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    saleType :: B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.IndirectTaxTransaction.SaleType),
    sgstAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    taxCreditType :: B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.IndirectTaxTransaction.GstCreditType),
    taxRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    taxableValue :: B.C f Kernel.Types.Common.HighPrecMoney,
    totalGstAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    totalTaxAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    transactionDate :: B.C f Kernel.Prelude.UTCTime,
    transactionType :: B.C f Lib.Finance.Domain.Types.IndirectTaxTransaction.TransactionType,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IndirectTaxTransactionT where
  data PrimaryKey IndirectTaxTransactionT f = IndirectTaxTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IndirectTaxTransactionId . id

type IndirectTaxTransaction = IndirectTaxTransactionT Identity

$(enableKVPG ''IndirectTaxTransactionT ['id] [['counterpartyId], ['invoiceNumber], ['referenceId], ['transactionDate]])

$(mkTableInstancesGenericSchema ''IndirectTaxTransactionT "indirect_tax_transaction")
