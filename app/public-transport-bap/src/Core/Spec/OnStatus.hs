module Core.Spec.OnStatus
  ( module Core.Spec.OnStatus,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.OnStatus.Order as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnStatusMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
