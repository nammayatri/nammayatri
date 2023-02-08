module Core.Spec.OnConfirm.Time where

import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Data.Time.Clock
import Kernel.Utils.Schema
import Relude

data Time = Time
  { label :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

instance ToSchema Time where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
