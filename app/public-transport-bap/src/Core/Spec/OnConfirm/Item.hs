module Core.Spec.OnConfirm.Item where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.OnConfirm.Quantity
import Data.OpenApi

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
