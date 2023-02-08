module Core.Spec.Common.ProviderId where

import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
  deriving (PrettyShow) via Showable ProviderId

instance ToSchema ProviderId where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
