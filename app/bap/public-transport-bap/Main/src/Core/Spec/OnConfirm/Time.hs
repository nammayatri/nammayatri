module Core.Spec.OnConfirm.Time where

import Beckn.Utils.Schema
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Data.Time.Clock
import Relude

data Time = Time
  { label :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

instance ToSchema Time where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
