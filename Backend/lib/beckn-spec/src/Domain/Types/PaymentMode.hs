module Domain.Types.PaymentMode (PaymentMode (..)) where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data PaymentMode = LIVE | TEST -- LIVE is default
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''PaymentMode)

$(mkHttpInstancesForEnum ''PaymentMode)
