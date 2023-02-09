module Beckn.Spec.OnConfirm.Item where

import Beckn.Spec.OnConfirm.Quantity
import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Item = Item
  { route_code :: Text,
    start_stop :: Text,
    end_stop :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
