module Core.Common.OrderState where

import Data.Aeson
import Relude

data State
  = ACTIVE
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)
