module Core.OnStatus where

import Data.Aeson
import Relude hiding (State, id, state)
import Core.OnStatus.Order

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
