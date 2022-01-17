module Core.OnConfirm
  ( module Core.OnConfirm,
    module Reexport,
  )
where

import Beckn.Prelude
import Core.OnConfirm.Billing as Reexport
import Core.OnConfirm.Breakup as Reexport
import Core.OnConfirm.Contact as Reexport
import Core.OnConfirm.Descriptor as Reexport
import Core.OnConfirm.End as Reexport
import Core.OnConfirm.Fulfillment as Reexport
import Core.OnConfirm.Item as Reexport
import Core.OnConfirm.Payment as Reexport
import Core.OnConfirm.Price as Reexport
import Core.OnConfirm.Provider as Reexport
import Core.OnConfirm.SpecQuote as Reexport
import Core.OnConfirm.Start as Reexport
import Core.OnConfirm.StartLocation as Reexport
import Core.OnConfirm.Time as Reexport
import Core.OnConfirm.Vehicle as Reexport
import Core.Order

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
