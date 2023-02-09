module Beckn.Spec.OnConfirm (module Beckn.Spec.OnConfirm, module Reexport) where

import Beckn.Spec.OnConfirm.Descriptor as Reexport
import Beckn.Spec.OnConfirm.Item as Reexport
import Beckn.Spec.OnConfirm.Order as Reexport
import Beckn.Spec.OnConfirm.Params as Reexport
import Beckn.Spec.OnConfirm.Quantity as Reexport
import Beckn.Spec.OnConfirm.Time as Reexport
import Kernel.Prelude
import Kernel.Utils.GenericPretty

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow, ToSchema)
