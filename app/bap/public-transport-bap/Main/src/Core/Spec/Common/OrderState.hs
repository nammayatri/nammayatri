module Core.Spec.Common.OrderState where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))

data State
  = ACTIVE
  | COMPLETE
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable State
