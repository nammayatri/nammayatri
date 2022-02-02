module Core.OnStatus where

import Core.OnStatus.Order
import Data.Aeson
import Relude hiding (State, id, state)

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
