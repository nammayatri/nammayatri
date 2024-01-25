{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Invoice where

import Data.Aeson
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Domain.Types.Plan (ServiceNames)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Invoice = Invoice
  { id :: Id Invoice,
    invoiceShortId :: Text,
    driverFeeId :: Id DF.DriverFee,
    driverId :: Id Person,
    invoiceStatus :: InvoiceStatus,
    paymentMode :: InvoicePaymentMode,
    bankErrorMessage :: Maybe Text,
    bankErrorCode :: Maybe Text,
    bankErrorUpdatedAt :: Maybe UTCTime,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    lastStatusCheckedAt :: Maybe UTCTime,
    maxMandateAmount :: Maybe HighPrecMoney,
    serviceName :: ServiceNames,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data InvoiceStatus = ACTIVE_INVOICE | INACTIVE | SUCCESS | FAILED | EXPIRED | CLEARED_BY_YATRI_COINS deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord)

data InvoicePaymentMode = MANUAL_INVOICE | AUTOPAY_INVOICE | MANDATE_SETUP_INVOICE | CASH_COLLECTED_INVOICE deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(mkBeamInstancesForEnum ''InvoiceStatus)
$(mkBeamInstancesForEnum ''InvoicePaymentMode)

$(mkHttpInstancesForEnum ''InvoiceStatus)
$(mkHttpInstancesForEnum ''InvoicePaymentMode)
