module Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..), PaymentChargeType (..)) where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data PaymentChargeBearer = PAYMENT_CUSTOMER | PAYMENT_DRIVER | PAYMENT_PLATFORM
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''PaymentChargeBearer)

$(mkHttpInstancesForEnum ''PaymentChargeBearer)

data PaymentChargeType = FIXED | PERCENTAGE
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''PaymentChargeType)

$(mkHttpInstancesForEnum ''PaymentChargeType)
