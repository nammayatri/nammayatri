module Core.Spec.Confirm.Item where

import Data.OpenApi hiding (items)
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

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
