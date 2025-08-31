{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Invoice where

import Data.Aeson
import qualified Domain.Types.DriverFee
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Invoice = Invoice
  { bankErrorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankErrorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankErrorUpdatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    driverFeeId :: Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.Invoice.Invoice,
    invoiceShortId :: Kernel.Prelude.Text,
    invoiceStatus :: Domain.Types.Invoice.InvoiceStatus,
    lastStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    maxMandateAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentMode :: Domain.Types.Invoice.InvoicePaymentMode,
    serviceName :: Domain.Types.Plan.ServiceNames,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InvoicePaymentMode
  = MANUAL_INVOICE
  | AUTOPAY_INVOICE
  | MANDATE_SETUP_INVOICE
  | CASH_COLLECTED_INVOICE
  | PAYOUT_REGISTRATION_INVOICE
  | ONE_TIME_SECURITY_INVOICE
  | ONE_TIME_SECURITY_ADJUSTED_INVOICE
  | PREPAID_INVOICE
  | WALLET_TOPUP_INVOICE
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data InvoiceStatus = ACTIVE_INVOICE | INACTIVE | SUCCESS | FAILED | EXPIRED | CLEARED_BY_YATRI_COINS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''InvoiceStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''InvoiceStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''InvoicePaymentMode)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''InvoicePaymentMode)
