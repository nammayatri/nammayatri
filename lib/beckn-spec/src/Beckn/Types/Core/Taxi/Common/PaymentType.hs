module Beckn.Types.Core.Taxi.Common.PaymentType where

import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.JSON (constructorsWithHyphens)
import Kernel.Utils.Schema

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens
