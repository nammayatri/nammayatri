module Core.Spec.OnConfirm (module Core.Spec.OnConfirm, module Reexport) where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Core.Spec.OnConfirm.Descriptor as Reexport
import Core.Spec.OnConfirm.Item as Reexport
import Core.Spec.OnConfirm.Order as Reexport
import Core.Spec.OnConfirm.Params as Reexport
import Core.Spec.OnConfirm.Quantity as Reexport
import Core.Spec.OnConfirm.Time as Reexport

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow, ToSchema)
