module Core.OnCancel where

import Data.Aeson
import Relude hiding (State, id, state)
import Core.OnCancel.Order

newtype OnCancelMessage = OnCancelMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
