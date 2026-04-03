{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Finance.Domain.Types.DirectTaxTransaction where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data DirectTaxTransaction
    = DirectTaxTransaction {counterpartyId :: Kernel.Prelude.Text,
                            createdAt :: Kernel.Prelude.UTCTime,
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
                            updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
data TdsRateReason = PAN | PAN_AADHAR_LINKAGE | LDC_CERTIFICATE | NO_PAN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data TdsTreatment = Deducted | Reimbursed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data TransactionType
    = RideFare | Subscription | Incentive | Cancellation | BuyerCommission | CreditNote | DebitNote | PGFee
    deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TdsRateReason))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TdsTreatment))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))

