module Core.Confirm
  ( module Core.Confirm,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Confirm.Billing as Reexport
import Core.Confirm.Fulfillment as Reexport
import Core.Confirm.Item as Reexport
import Core.Confirm.Order as Reexport
import Core.Confirm.Provider as Reexport
import Core.Confirm.StartEnd as Reexport
import Core.Confirm.Time as Reexport
import Core.Confirm.Vehicle as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToSchema ConfirmMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
