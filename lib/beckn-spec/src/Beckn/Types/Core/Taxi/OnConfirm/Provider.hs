module Beckn.Types.Core.Taxi.OnConfirm.Provider where

import Beckn.Utils.Example (Example (..))
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude

newtype Provider = Provider
  { name :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Provider where
  example =
    Provider
      { name = "Yatri Taxi"
      }
