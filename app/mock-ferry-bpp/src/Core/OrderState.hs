module Core.OrderState where

import Beckn.Prelude

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)
