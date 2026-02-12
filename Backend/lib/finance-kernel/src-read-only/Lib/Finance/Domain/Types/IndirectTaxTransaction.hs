{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.IndirectTaxTransaction where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data IndirectTaxTransaction = IndirectTaxTransaction
  { cgstAmount :: Kernel.Types.Common.HighPrecMoney,
    counterpartyId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    creditOrDebitNoteNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstCreditType :: Lib.Finance.Domain.Types.IndirectTaxTransaction.GstCreditType,
    gstRate :: Kernel.Prelude.Double,
    gstinOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction,
    igstAmount :: Kernel.Types.Common.HighPrecMoney,
    invoiceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Text,
    sacCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    saleType :: Lib.Finance.Domain.Types.IndirectTaxTransaction.SaleType,
    sgstAmount :: Kernel.Types.Common.HighPrecMoney,
    taxableValue :: Kernel.Types.Common.HighPrecMoney,
    totalGstAmount :: Kernel.Types.Common.HighPrecMoney,
    transactionDate :: Kernel.Prelude.UTCTime,
    transactionType :: Lib.Finance.Domain.Types.IndirectTaxTransaction.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data GstCreditType = Input | Output deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SaleType = B2B | B2C deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransactionType = RideFare | Subscription | Incentive | Cancellation | BuyerCommission | CreditNote | DebitNote deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)


$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''GstCreditType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SaleType))
