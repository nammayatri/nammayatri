module Core.OnConfirm
  ( module Core.OnConfirm,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnConfirm.Fulfillment as Reexport
import Core.OnConfirm.Item as Reexport
import Core.OnConfirm.Location as Reexport
import Core.OnConfirm.Order as Reexport
import Core.OnConfirm.Provider as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnConfirmMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
