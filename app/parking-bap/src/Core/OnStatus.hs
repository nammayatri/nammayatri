module Core.OnStatus
  ( module Core.OnStatus,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnStatus.Fulfillment as Reexport
import Core.OnStatus.Item as Reexport
import Core.OnStatus.Order as Reexport
import Core.OnStatus.Provider as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnStatusMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
