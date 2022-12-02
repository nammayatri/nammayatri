module Core.Spec.Common.ProviderId where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
  deriving (PrettyShow) via Showable ProviderId

instance ToSchema ProviderId where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
