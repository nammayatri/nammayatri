module Core.Spec.Common.OrderState where

import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))

data State
  = ACTIVE
  | COMPLETE
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable State
