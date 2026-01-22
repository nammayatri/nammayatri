{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverFee (module Domain.Types.DriverFee, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.DriverFee as ReExport
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DriverFee = DriverFee
  { addedToFeeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee),
    amountPaidByCoin :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    autopayPaymentStage :: Kernel.Prelude.Maybe Domain.Types.DriverFee.AutopayPaymentStage,
    badDebtDeclarationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    badDebtRecoveryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    billNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationPenaltyAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    collectedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    collectedAtVendorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    collectedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    driverConsideredInPayoutSettlementAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Driver,
    endTime :: Kernel.Prelude.UTCTime,
    feeType :: Domain.Types.DriverFee.FeeType,
    feeWithoutDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    govtCharges :: Kernel.Types.Common.HighPrecMoney,
    hasSibling :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notificationRetryCount :: Kernel.Prelude.Int,
    numRides :: Kernel.Prelude.Int,
    offerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    overlaySent :: Kernel.Prelude.Bool,
    payBy :: Kernel.Prelude.UTCTime,
    planId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan),
    planMode :: Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode,
    planOfferTitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platformFee :: Domain.Types.DriverFee.PlatformFee,
    refundEntityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    refundedBy :: Kernel.Prelude.Maybe Domain.Types.DriverFee.RefundedBy,
    schedulerTryCount :: Kernel.Prelude.Int,
    serviceName :: Domain.Types.Plan.ServiceNames,
    siblingFeeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee),
    specialZoneAmount :: Kernel.Types.Common.HighPrecMoney,
    specialZoneRideCount :: Kernel.Prelude.Int,
    splitOfDriverFeeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee),
    stageUpdatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.DriverFee.DriverFeeStatus,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    validDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, Eq)

data AutopayPaymentStage
  = NOTIFICATION_SCHEDULED
  | NOTIFICATION_ATTEMPTING
  | EXECUTION_SCHEDULED
  | EXECUTION_ATTEMPTING
  | EXECUTION_SUCCESS
  | EXECUTION_FAILED
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data DriverFeeStatus
  = ONGOING
  | PAYMENT_PENDING
  | PAYMENT_OVERDUE
  | CLEARED
  | EXEMPTED
  | COLLECTED_CASH
  | INACTIVE
  | CLEARED_BY_YATRI_COINS
  | MANUAL_REVIEW_NEEDED
  | REFUND_PENDING
  | REFUNDED
  | REFUND_FAILED
  | REFUND_MANUAL_REVIEW_REQUIRED
  | ONE_TIME_SECURITY_ADJUSTED
  | SETTLED
  | IN_DISPUTE_WINDOW
  | ADDED_TO_INVOICE
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data FeeType
  = MANDATE_REGISTRATION
  | RECURRING_INVOICE
  | RECURRING_EXECUTION_INVOICE
  | PAYOUT_REGISTRATION
  | ONE_TIME_SECURITY_DEPOSIT
  | PREPAID_RECHARGE
  | WALLET_TOPUP
  | CANCELLATION_PENALTY
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord, Enum)

data PlatformFee = PlatformFee {cgst :: Kernel.Types.Common.HighPrecMoney, currency :: Kernel.Types.Common.Currency, fee :: Kernel.Types.Common.HighPrecMoney, sgst :: Kernel.Types.Common.HighPrecMoney}
  deriving (Generic, Eq, Show)

data RefundInfo = RefundInfo
  { refundEntityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    refundedBy :: Kernel.Prelude.Maybe Domain.Types.DriverFee.RefundedBy,
    status :: Kernel.Prelude.Maybe Domain.Types.DriverFee.DriverFeeStatus
  }
  deriving (Generic, Eq, Show)

data RefundedBy = PAYOUT | REFUNDS_API deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''DriverFeeStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''FeeType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''DriverFeeStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''FeeType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''AutopayPaymentStage)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''RefundedBy)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''RefundedBy)
