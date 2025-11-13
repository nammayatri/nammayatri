module SharedLogic.Type where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

-- import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data BillingCategory = PERSONAL | BUSINESS deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BillingCategory)

$(mkHttpInstancesForEnum ''BillingCategory)
