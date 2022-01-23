module Core.OrderState where

import Data.Aeson
import Relude

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)
