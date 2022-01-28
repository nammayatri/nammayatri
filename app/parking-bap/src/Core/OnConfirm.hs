module Core.OnConfirm
  ( module Core.OnConfirm,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Billing as Reexport
import Core.Breakup as Reexport
import Core.Contact as Reexport
import Core.Descriptor as Reexport
import Core.End as Reexport
import Core.Fulfillment as Reexport
import Core.Item as Reexport
import Core.Order
import Core.Payment as Reexport
import Core.Price as Reexport
import Core.Provider as Reexport
import Core.SpecQuote as Reexport
import Core.Start as Reexport
import Core.StartLocation as Reexport
import Core.Time as Reexport
import Core.Vehicle as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnConfirmMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
