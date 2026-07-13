{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.DirectTaxTransaction where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Core.Types
import qualified Tools.Beam.UtilsTH

data DirectTaxTransaction = DirectTaxTransaction
  { counterpartyId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType,
    createdById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    grossAmount :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction,
    invoiceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    netAmountPaid :: Kernel.Types.Common.HighPrecMoney,
    panOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referenceId :: Kernel.Prelude.Text,
    tanOfDeductee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tdsAmount :: Kernel.Types.Common.HighPrecMoney,
    tdsRate :: Kernel.Prelude.Double,
    tdsRateReason :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.DirectTaxTransaction.TdsRateReason,
    tdsSection :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tdsTreatment :: Lib.Finance.Domain.Types.DirectTaxTransaction.TdsTreatment,
    transactionDate :: Kernel.Prelude.UTCTime,
    transactionType :: Lib.Finance.Domain.Types.DirectTaxTransaction.TransactionType,
    updatedBy :: Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType,
    updatedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data TdsRateReason = PAN | PAN_AADHAR_LINKAGE | LDC_CERTIFICATE | NO_PAN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TdsTreatment = Deducted | Reimbursed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransactionType
  = RideFare
  | Subscription
  | Incentive
  | Cancellation
  | BuyerCommission
  | CreditNote
  | DebitNote
  | PGFee
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TdsRateReason))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TdsTreatment))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))
