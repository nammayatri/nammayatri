module Core.Status where

import Data.Aeson
import Relude hiding (id)

newtype StatusMessage = StatusMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Order = Order
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
