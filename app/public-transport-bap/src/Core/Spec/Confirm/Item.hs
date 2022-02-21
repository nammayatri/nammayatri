module Core.Spec.Confirm.Item where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi hiding (items)

data Item = Item
  { route_code :: Text,
    start_stop :: Text,
    end_stop :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
