{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BBPS where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data BBPS = BBPS
  { amount :: Kernel.Types.Common.HighPrecMoney,
    billerId :: Kernel.Prelude.Text,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    customerMobileNumber :: Kernel.Prelude.Text,
    customerParams :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentInformation :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentMode,
    paymentTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refId :: Kernel.Types.Id.Id Domain.Types.BBPS.BBPS,
    refShortId :: Kernel.Types.Id.ShortId Domain.Types.BBPS.BBPS,
    status :: Domain.Types.BBPS.BBPSPaymentStatus,
    transType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BBPSPaymentMode = UPI | Debit_Card | Credit_Card | Others deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data BBPSPaymentStatus
  = NEW
  | PENDING
  | SUCCESS
  | FAILED
  | REFUND_PENDING
  | REFUND_INITIATED
  | REFUND_FAILED
  | REFUNDED
  | CONFIRMATION_PENDING
  | CONFIRMATION_FAILED
  | AWAITING_BBPS_CONFIRMATION
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data BBPSSessionPayload = BBPSSessionPayload {token :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Tag = Tag {name :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BBPSPaymentMode))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BBPSPaymentStatus))

$(mkHttpInstancesForEnum (''BBPSPaymentStatus))
