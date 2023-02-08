module Beckn.Types.Core.Taxi.OnConfirm.Provider where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Kernel.Utils.Example (Example (..))
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

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
