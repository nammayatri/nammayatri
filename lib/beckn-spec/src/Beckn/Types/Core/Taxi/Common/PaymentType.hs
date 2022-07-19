module Beckn.Types.Core.Taxi.Common.PaymentType where

import Beckn.Prelude
import Beckn.Utils.JSON (constructorsWithHyphens)
import Beckn.Utils.Schema
import Data.OpenApi

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
