module Core.OnCancel where

import Core.OnCancel.Order
import Data.Aeson
import Relude hiding (State, id, state)

newtype OnCancelMessage = OnCancelMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
