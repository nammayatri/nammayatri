{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.DirectTaxTransaction where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data DirectTaxTransaction = DirectTaxTransaction
  { counterpartyId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    grossAmount :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction,
    invoiceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    netAmountPaid :: Kernel.Types.Common.HighPrecMoney,
    panOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referenceId :: Kernel.Prelude.Text,
    tanOfDeductee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tdsAmount :: Kernel.Types.Common.HighPrecMoney,
    tdsRate :: Kernel.Prelude.Double,
    tdsTreatment :: Lib.Finance.Domain.Types.DirectTaxTransaction.TdsTreatment,
    transactionDate :: Kernel.Prelude.UTCTime,
    transactionType :: Lib.Finance.Domain.Types.DirectTaxTransaction.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data TdsTreatment = Deducted | Reimbursed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransactionType = RideFare | Subscription | Incentive | Cancellation | BuyerCommission | CreditNote | DebitNote deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TdsTreatment))
